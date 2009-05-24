package org.singingwizard.raycasting.test

import scala.actors._
import Actor._
import scala.swing._
import scala.swing.event._
import java.awt.event.{KeyListener, KeyEvent}
import java.awt.{Dimension, Graphics2D, Graphics}
import java.awt.{Image}
import java.awt.{Color => AWTColor}

import engine.RayCaster._

object SwingTest extends SimpleGUIApplication {
  var rootPanel : Panel = null 
  def top = new MainFrame {
    title = "Ray Caster"
    minimumSize = new Dimension(640*2 + 40, 480+100)

    peer.addKeyListener(new KeyListener {
      override def keyPressed(ev:KeyEvent) {
        val SetPosition(Position(x,y), angle) = renderer !? GetPosition
        //println(SetPosition(Position(x,y), angle) )
        renderer ! (ev.getKeyCode match {
          case 39 => SetPosition(Position(x,y), angle+0.1)
          case 37 => SetPosition(Position(x,y), angle-0.1)
          case 38 => SetPosition(Position((x+Math.cos(angle)*5).toInt,(y+Math.sin(angle)*5).toInt), angle)
          case 40 => SetPosition(Position((x-Math.cos(angle)*5).toInt,(y-Math.sin(angle)*5).toInt), angle)
        })
      
      } 
      override def keyReleased(ev:KeyEvent) {} 
      override def keyTyped(ev:KeyEvent) {} 
    })
    
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
          
          for( x <- 0 until level.widthCells ) {
            g2.drawLine(x*level.cellSize, 0, x*level.cellSize, level.heightCells*level.cellSize)
          }
          for( y <- 0 until level.heightCells ) {
            g2.drawLine(0, y*level.cellSize, level.widthCells*level.cellSize, y*level.cellSize )
          }
          
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
            g2.setColor(AWTColor.RED)
            drawPoints(vPoses)
          }
          
          ()
        }
      }
    }
    contents = rootPanel 
  }

  case class SetPosition(pos:Position, angle:Double)
  case object GetPosition
  case object GetImage
  case object GetDebugInfo
  case object GetLevel
  
  val renderer = actor {
	val b = new LevelBuilder(10, 10)
	val C = new Color(0,1,1)
	val R = new Color(1,0,0)
	val B = new Color(0,0,1)
	val G = new Color(0,1,0)
	b(0,0) = C
	b(9,0) = G
	b(9,1) = G
	b(9,2) = G
	b(9,3) = G
	b(9,4) = G
	b(3,3) = B
	b(4,3) = B
	b(4,5) = R
	b(3,5) = R
	b(2,5) = R
	println(b(0,0))
 
 
	/*b << 
		C|0|0|0|0|0|0|0|0|G|/
		C|0|0|0|0|0|0|0|0|G|/
		0|0|0|0|0|0|0|0|0|G|/
		C|0|0|0|0|0|0|0|0|G
		C|0|0|0|0|0|0|0|0|G|/
		0|0|0|0|0|B|B|0|0|G|/
		C|0|0|0|0|0|B|0|0|G|/
		C|0|0|0|B|0|R|0|0|G|/
		C|0|0|0|B|R|R|0|0|G|/
		C|0|0|0|0|0|0|0|0|G */
 
	val l = new Level(b)
	val r = new Renderer(l, Math.toRadians(60), 277, 640, 480)
 
    var pos = Position(142,156)
    //Position(73,103)
    var angle = -1.747197551196598;
    //-4.447197551196599
    var renderer = r
    var image : Image = renderer.render(pos, angle)
    loop {reactWithin(500) {
      case SetPosition(_pos, _angle) => pos = _pos; angle = _angle; image = renderer.render(pos, angle); rootPanel.repaint
      case GetPosition => reply( SetPosition(pos, angle) )
      case GetImage => reply(image)
      case GetDebugInfo => reply(renderer.debugInfo)
      case GetLevel => reply(l)
      case TIMEOUT => //self ! SetPosition(pos, angle + Math.toRadians(10)); rootPanel.repaint 
    }}
  }
}
