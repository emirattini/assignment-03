package pcd.ass03

import javax.swing.*
import javax.swing.event.ChangeEvent
import javax.swing.event.ChangeListener
import java.awt.*
import java.util
import akka.actor.typed.ActorSystem
import pcd.ass01.BoidsModel

import java.awt.event.ActionEvent
import java.util.Hashtable

enum ControlCommand:
  case Start, Stop, Suspend, Resume

  def getText: String = this match
    case ControlCommand.Start => "start"
    case ControlCommand.Stop => "stop"
    case ControlCommand.Suspend => "suspend"
    case ControlCommand.Resume => "resume"


class BoidsView(var model: BoidsModel, var width: Int, var height: Int) extends ChangeListener:
  private val frame = new JFrame("Boids Simulation")
  private val cp = new JPanel
  private val boidsPanel = new BoidsPanel(this, model)
  private val cohesionSlider: JSlider = makeSlider
  private val separationSlider: JSlider = makeSlider
  private val alignmentSlider: JSlider = makeSlider
  private val numBoidsField: JTextField = makeBoidsNumberField
  private val startButton: JButton = makeStartAndStopButton
  private val suspendResumeButton: JButton = makeSuspendResumeButton
  private var lastNumBoid = "1500"

  frame.setSize(width, height)
  frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  cp.setLayout(new BorderLayout)
  cp.add(BorderLayout.CENTER, boidsPanel)
  cp.add(BorderLayout.SOUTH, makeControlPanel)
  frame.setContentPane(cp)
  frame.setVisible(true)

  private var actorSystem: Option[ActorSystem[ControllerMessage]] = None

  def setActorSystem(actorSystem: ActorSystem[ControllerMessage]): Unit =
    this.actorSystem = Some(actorSystem)

  private def makeControlPanel = {
    val controlPanel = new JPanel
    this.disableSuspendResumeButton()
    this.resetBoidsNumberField()
    controlPanel.add(new JLabel("Separation"))
    controlPanel.add(separationSlider)
    controlPanel.add(new JLabel("Alignment"))
    controlPanel.add(alignmentSlider)
    controlPanel.add(new JLabel("Cohesion"))
    controlPanel.add(cohesionSlider)
    controlPanel.add(new JLabel("Count"))
    controlPanel.add(numBoidsField)
    controlPanel.add(startButton)
    controlPanel.add(suspendResumeButton)
    controlPanel
  }

  private def makeStartAndStopButton = {
    val button = new JButton(ControlCommand.Start.getText)
    button.setEnabled(false)
    button.addActionListener((e: ActionEvent) => {
      val btnText = button.getText
      if (btnText == ControlCommand.Start.getText) {
        val inputText = numBoidsField.getText
        try {
          val newBoidsNumber = inputText.toInt
          if (newBoidsNumber <= 0) throw new IllegalArgumentException
          startAction(newBoidsNumber)
        } catch {
          case ex1: NumberFormatException =>
            System.out.println("Input format not allowed!")
          case ex2: IllegalArgumentException =>
            System.out.println("Only positive numbers allowed!")
        }
      }
      else if (btnText == ControlCommand.Stop.getText) stopAction()

    })
    button
  }

  private def makeSuspendResumeButton = {
    val button = new JButton(ControlCommand.Suspend.getText)
    button.addActionListener((e: ActionEvent) => {
      val btnText = button.getText
      if (btnText == ControlCommand.Suspend.getText) suspendAction()
      else if (btnText == ControlCommand.Resume.getText) resumeAction()

    })
    button
  }

  private def stopAction(): Unit = {
    this.disableStartAndStopButton()
    this.enableNumBoidsField()
    actorSystem.foreach(_ ! ControlCommand.Stop)
    this.resetBoidsNumberField()
    startButton.setText(ControlCommand.Start.getText)
    this.disableSuspendResumeButton()
  }

  private def startAction(newBoidsNumber: Int): Unit = {
    this.disableStartAndStopButton()
    this.disableNumBoidsField()
    model.setBoidsNumber(newBoidsNumber)
    actorSystem.foreach(_ ! ControlCommand.Start)
    lastNumBoid = numBoidsField.getText
    numBoidsField.setText("")
    startButton.setText(ControlCommand.Stop.getText)
    this.enableSuspendResumeButton()
  }

  def resumeAction(): Unit = {
    this.disableSuspendResumeButton()
    suspendResumeButton.setText(ControlCommand.Suspend.getText)
    actorSystem.foreach(_ ! ControlCommand.Resume)
  }

  private def suspendAction(): Unit = {
    this.disableSuspendResumeButton()
    suspendResumeButton.setText(ControlCommand.Resume.getText)
    actorSystem.foreach(_ ! ControlCommand.Suspend)
  }

  private def enableNumBoidsField(): Unit = {
    numBoidsField.setEnabled(true)
  }

  private def disableNumBoidsField(): Unit = {
    numBoidsField.setEnabled(false)
  }

  def enableStartStopButton(): Unit = {
    startButton.setEnabled(true)
  }

  private def disableStartAndStopButton(): Unit = {
    startButton.setEnabled(false)
  }

  def enableSuspendResumeButton(): Unit = {
    suspendResumeButton.setEnabled(true)
  }

  private def disableSuspendResumeButton(): Unit = {
    suspendResumeButton.setEnabled(false)
  }

  private def makeBoidsNumberField = new JTextField(5)

  private def resetBoidsNumberField(): Unit = {
    //		numBoidsField.setText(Integer.toString(BoidsSimulation.N_BOIDS));
    numBoidsField.setText(lastNumBoid)
  }

  private def makeSlider = {
    val slider = new JSlider(SwingConstants.HORIZONTAL, 0, 20, 10)
    slider.setMajorTickSpacing(10)
    slider.setMinorTickSpacing(1)
    slider.setPaintTicks(true)
    slider.setPaintLabels(true)
    val labelTable = new util.Hashtable[Integer, JLabel]
    labelTable.put(0, new JLabel("0"))
    labelTable.put(10, new JLabel("1"))
    labelTable.put(20, new JLabel("2"))
    slider.setLabelTable(labelTable)
    slider.setPaintLabels(true)
    slider.addChangeListener(this)
    slider
  }

  def update(): Unit = {
    boidsPanel.repaint()
  }

  def updateFrameRate(frameRate: Int): Unit = {
    boidsPanel.setFrameRate(frameRate)
  }

  override def stateChanged(e: ChangeEvent): Unit = {
    if (e.getSource eq separationSlider) {
      val `val` = separationSlider.getValue
      model.setSeparationWeight(0.1 * `val`)
    }
    else if (e.getSource eq cohesionSlider) {
      val `val` = cohesionSlider.getValue
      model.setCohesionWeight(0.1 * `val`)
    }
    else {
      val `val` = alignmentSlider.getValue
      model.setAlignmentWeight(0.1 * `val`)
    }
  }

  def getWidth: Int = width

  def getHeight: Int = height

import javax.swing._
import java.awt._

class BoidsPanel(private var view: BoidsView, private var model: BoidsModel) extends JPanel {
  private var framerate = 0

  def setFrameRate(framerate: Int): Unit = {
    this.framerate = framerate
  }

  override protected def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)
    setBackground(Color.WHITE)
    val w = view.getWidth
    val h = view.getHeight
    val envWidth = model.getWidth
    val xScale = w / envWidth
    // var envHeight = model.getHeight();
    // var yScale = h/envHeight;
    import scala.jdk.CollectionConverters.*
    val boids = model.getBoids.asScala
    g.setColor(Color.BLUE)
    for (boid <- boids) {
      val x = boid.getPos.x
      val y = boid.getPos.y
      val px = (w / 2 + x * xScale).toInt
      val py = (h / 2 - y * xScale).toInt
      g.fillOval(px, py, 5, 5)
    }
    g.setColor(Color.BLACK)
    g.drawString("Num. Boids: " + boids.size, 10, 25)
    g.drawString("Framerate: " + framerate, 10, 40)
  }
}
