package pcd.ass03

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import pcd.ass01.{Boid, BoidsModel}
import pcd.ass03.BoidCommand.*
import pcd.ass03.ControlCommand.*

import scala.jdk.CollectionConverters.*

enum BoidCommand:
  case UpdateVelocity(model: BoidsModel)
  case UpdatePosition(model: BoidsModel)

object BoidActor:
  def apply(boid: Boid, controller: ActorRef[ControllerMessage]): Behavior[BoidCommand] =
    Behaviors.setup: ctx =>
      Behaviors.receiveMessage:
        case UpdateVelocity(model) =>
          boid.updateVelocity(model)
          controller ! BoidMessage.VelocityUpdated
          Behaviors.same
        case UpdatePosition(model) =>
          boid.updatePos(model)
          controller ! BoidMessage.PositionUpdated(boid)
          Behaviors.same

enum BoidMessage:
  case VelocityUpdated
  case PositionUpdated(boid: Boid)

type ControllerMessage = ControlCommand | BoidMessage

import pcd.ass03.BoidMessage.*

object ControllerActor:
  def apply(model: BoidsModel, view: BoidsView): Behavior[ControllerMessage] =
    waiting(model, view)

  private def waiting(model: BoidsModel, view: BoidsView): Behavior[ControllerMessage] =
    println("WAITING state")
    view.enableStartStopButton()
    Behaviors.receivePartial:
      case (ctx, ControlCommand.Start) =>
        start(model, view, ctx)

  private def start(model: BoidsModel, view: BoidsView,
                    ctx: ActorContext[ControllerMessage]): Behavior[ControllerMessage] =
    println("Initializing boid actors")
    model.generateBoids()
    val boidActors = for
      (boid, index) <- model.getBoids.asScala.zipWithIndex
    yield ctx.spawnAnonymous(BoidActor(new Boid(boid), ctx.self))
    boidActors.foreach(_ ! BoidCommand.UpdateVelocity(model))
    model.setStartingTime(System.currentTimeMillis())
    waitingVelocitiesUpdate(model, view, boidActors.toSeq)

  private def waitingVelocitiesUpdate(model: BoidsModel, view: BoidsView,
                                      boidActors: Seq[ActorRef[BoidCommand]]): Behavior[ControllerMessage] =
    println("WAITING_VELOCITIES_UPDATE state")
    view.enableStartStopButton()
    view.enableSuspendResumeButton()
    Behaviors.withStash[ControllerMessage](5): stash =>
      var velocitiesUpdated = 0
      Behaviors.receiveMessagePartial:
        case VelocityUpdated =>
          velocitiesUpdated += 1
          if velocitiesUpdated == model.getBoids.size()
          then
            println("All velocities updated.")
            if stash.isEmpty
            then
              println("No control commands sent. Updating positions...")
              boidActors.foreach(_ ! BoidCommand.UpdatePosition(model))
              waitingPositionsUpdate(model, view, boidActors)
            else
              println("Handling control commands...")
              stash.unstashAll(handleControlCommands(model, view, boidActors))
          else
            Behaviors.same
        case other =>
          stash.stash(other)
          Behaviors.same


  private def waitingPositionsUpdate(model: BoidsModel, view: BoidsView,
                                     boidActors: Seq[ActorRef[BoidCommand]]): Behavior[ControllerMessage] =
    println("WAITING_POSITIONS_UPDATE state")
    Behaviors.withStash[ControllerMessage](5): stash =>
      var positionsUpdated = 0
      var updatedBoids: Seq[Boid] = Seq()
      Behaviors.receivePartial:
        case (ctx, PositionUpdated(boid)) =>
          positionsUpdated += 1
          updatedBoids = updatedBoids :+ boid
          if positionsUpdated == model.getBoids.size()
          then
            println("All positions updated")
            update(model, view, updatedBoids)
            if stash.isEmpty
            then
              println("No control commands sent. Sending new update...")
              boidActors.foreach(_ ! BoidCommand.UpdateVelocity(model))
              waitingVelocitiesUpdate(model, view, boidActors)
            else
              println("Handling control commands...")
              stash.unstashAll(handleControlCommands(model, view, boidActors))
          else Behaviors.same
        case (_, other) =>
          stash.stash(other)
          Behaviors.same

  private def update(model: BoidsModel, view: BoidsView, updatedBoids: Seq[Boid]): Unit =
    model.setBoids(updatedBoids.asJavaCollection)
    val elapsed = model.getElapsedTime
    println(s"Elapsed: $elapsed ms")
    val frameratePeriod = 1000 / model.getMaxFramerate
    println(s"Framerate period: $frameratePeriod ms")
    if elapsed < frameratePeriod
    then
      println("Setting max fps")
      Thread.sleep(frameratePeriod - elapsed)
      model.setFramerate(model.getMaxFramerate)
    else
      println("Setting custom fps")
      model.setFramerate((1000 / elapsed).toInt)
    view.update()
    view.updateFrameRate(model.getFramerate)
    model.setStartingTime(System.currentTimeMillis())

  private def handleControlCommands(model: BoidsModel, view: BoidsView,
                                    boidActors: Seq[ActorRef[BoidCommand]]): Behavior[ControllerMessage] =
    Behaviors.receiveMessagePartial:
      case Stop =>
        println("Stopping simulation")
        model.clearBoids()
        view.update()
        view.updateFrameRate(0)
        waiting(model, view)
      case Suspend =>
        println("Suspending simulation")
        view.enableSuspendResumeButton()
        suspended(model, view, boidActors)

  private def suspended(model: BoidsModel, view: BoidsView, boidActors: Seq[ActorRef[BoidCommand]]): Behavior[ControllerMessage] =
    println("SUSPENDED state")
    Behaviors.receivePartial:
      case (ctx, Resume) =>
        println("Resuming simulation")
        boidActors.foreach(_ ! BoidCommand.UpdateVelocity(model))
        model.setStartingTime(System.currentTimeMillis())
        waitingVelocitiesUpdate(model, view, boidActors)
      case (_, Stop) =>
        println("Stopping simulation")
        waiting(model, view)