package org.singingwizard.raycasting.test

import scala.actors._
import Actor._
import scala.swing._
import scala.swing.event._
import java.awt.{Dimension, Graphics2D, Graphics}
import java.awt.{Image}
import java.awt.{Color => AWTColor}

import engine.RayCaster._

object SwingTest extends SimpleGUIApplication {
  var rootPanel : Panel = null 
  def top = new MainFrame {
    title = "Ray Caster"
    minimumSize = new Dimension(640*2 + 40, 480+100)
    rootPanel = new BoxPanel(Orientation.Horizontal) {
      background = AWTColor.BLACK
      
      // Render output
      contents += new Panel {
        background = AWTColor.GRAY
        minimumSize = new Dimension(640, 480)
        preferredSize = minimumSize
        override def paintComponent(g : Graphics) {
          val img = renderer !! (GetImage, {case v : Image => v})
          super.paintComponent(g)
          val g2 = g.asInstanceOf[Graphics2D]
          g2.drawImage(img(), 0,0, null)
          ()
        }
      }
      
      contents += new Separator(Orientation.Vertical)
      
      // Debug output
      contents += new Panel {
    	background = AWTColor.GRAY
        minimumSize = new Dimension(640, 480)
        preferredSize = minimumSize
        
        override def paintComponent(g : Graphics) {
          val infoFut = renderer !! (GetDebugInfo, {case v : DebugInfo => v})
          val levelFut = renderer !! (GetLevel, {case v : Level => v})
          
          super.paintComponent(g)
          val g2 = g.asInstanceOf[Graphics2D]
          def drawPoint(x:Int,y:Int) = g2.drawLine(x,y, x,y)
          def drawPoints(poses : List[Position]) = {
            for( Position(x,y) <- poses )
              drawPoint(x,y)
          }
          
          val level = levelFut()
          for( x <- 0 until level.widthCells; y <- 0 until level.heightCells ) {
            level(x, y) match {
              case Some(Color(r,g,b)) => { 
                g2.setColor(new AWTColor(r.toFloat, g.toFloat, b.toFloat))
                g2.drawRect(x*level.cellSize, y*level.cellSize, level.cellSize, level.cellSize)
              }
              case Some(_) => { 
                g2.setColor(AWTColor.WHITE)
                g2.drawRect(x*level.cellSize, y*level.cellSize, level.cellSize, level.cellSize)
              }
              case None =>
            }
          }
          
          val info = infoFut()
          
          g2.setColor(AWTColor.RED)
       	  val Position(x, y) = info.position
       	  g2.drawOval(x, y, 5, 5)
          import Math.{cos, sin}
       	  g2.drawLine(x+3, y+2, (x+3 + cos(info.angle)*30).toInt, (y+2 + sin(info.angle)*30).toInt)
          for( RayInfo(_, hPoses, vPoses) <- info.rays ) {
        	g2.setColor(AWTColor.YELLOW)
            drawPoints(hPoses)
            g2.setColor(AWTColor.MAGENTA)
            drawPoints(vPoses)
          }
          
          ()
        }
      }
    }
    contents = rootPanel 
  }

  case class SetPosition(pos:Position, angle:Double)
  case object GetImage
  case object GetDebugInfo
  case object GetLevel
  
  val renderer = actor {
	val b = new LevelBuilder(10, 10)
	b(0,0) = new Color(0,1,1)
	println(b(0,0))
 
	val l = new Level(b)
	val r = new Renderer(l, Math.toRadians(60), 277, 640, 480)
 
    var pos = Position(130, 130)
    var angle = 0.0
    var renderer = r
    var image : Image = renderer.render(pos, angle)
    loop {reactWithin(500) {
      case SetPosition(_pos, _angle) => pos = _pos; angle = _angle; image = renderer.render(pos, angle)
      case GetImage => reply(image)
      case GetDebugInfo => reply(renderer.debugInfo)
      case GetLevel => reply(l)
      case TIMEOUT => self ! SetPosition(pos, angle + Math.toRadians(10)); rootPanel.repaint 
    }}
  }
}
