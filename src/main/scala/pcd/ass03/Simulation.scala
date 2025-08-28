package pcd.ass03

import akka.actor.typed.ActorSystem
import pcd.ass01.BoidsModel

val N_BOIDS = 3000
val SEPARATION_WEIGHT = 1.0
val ALIGNMENT_WEIGHT = 1.0
val COHESION_WEIGHT = 1.0

val ENVIRONMENT_WIDTH = 1000
val ENVIRONMENT_HEIGHT = 1000
val MAX_SPEED = 4.0
val PERCEPTION_RADIUS = 50.0
val AVOID_RADIUS = 20.0

val SCREEN_WIDTH = 1280
val SCREEN_HEIGHT = 720

val FRAMERATE = 50

@main def runSimulation(): Unit =
  val model = new BoidsModel(N_BOIDS, SEPARATION_WEIGHT, ALIGNMENT_WEIGHT,
    COHESION_WEIGHT, ENVIRONMENT_WIDTH, ENVIRONMENT_HEIGHT, MAX_SPEED,
    PERCEPTION_RADIUS, AVOID_RADIUS, FRAMERATE)
  val view = new BoidsView(model, SCREEN_WIDTH, SCREEN_HEIGHT)
  val system = ActorSystem(ControllerActor(model, view), "Controller")
  view.setActorSystem(system)
