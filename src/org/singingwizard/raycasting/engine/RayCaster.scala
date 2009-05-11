package org.singingwizard.raycasting.engine

import java.awt.image.BufferedImage
import java.awt.Graphics2D
import java.awt.BasicStroke
//import java._

object RayCaster {
  abstract class Texture extends NotNull
  case class Color(red:Double, green:Double, blue:Double) extends Texture
  
  class LevelBuilder(val width : Int, val height : Int) {
    private[engine] val data = new Array[Option[Texture]](width * height)
   
    def apply(x:Int, y:Int) : Option[Texture] = {
      val v = data(x + y*width)
      if( v == null )
        None
      else 
        v
    }
    def update(x:Int, y:Int, v : Texture) : Unit = this(x, y) = Some(v)
    def update(x:Int, y:Int, v : Some[Texture]) : Unit = data(x + y*width) = v
    
    var cellSize : Int = 64
    var wallTexture : Texture = Color(1,1,1)
  }
  
  final class Level( private[this] val levelBuilder : LevelBuilder ) {
    private[this] val data = new Array[Option[Texture]](levelBuilder.data.length)
    Array.copy(levelBuilder.data, 0, data, 0, data.length)
    val cellSize = levelBuilder.cellSize
    val widthCells = levelBuilder.width
    val heightCells = levelBuilder.height
    val width = widthCells*cellSize
    val height = heightCells*cellSize
    val wallTexture = levelBuilder.wallTexture
    
    def apply(x:Int, y:Int) : Option[Texture] = {
      val v = data(x + y*widthCells)
      if( v == null )
        None
      else 
        v
    }
  }
  
  case class Position( x:Int, y:Int )
  
  case class RayInfo(angle : Double, vPositions : List[Position], hPositions : List[Position])
  case class DebugInfo(position : Position, angle : Double, rays : List[RayInfo])
  
  class Renderer(val level : Level, val fov:Double, val projectionPlaneDistance :Int, val outWidth:Int, val outHeight:Int) {
    import java.lang.Math._
      
    private[this] def castRay(xPos:Int, yPos:Int, xStep:Int, yStep:Int) : (Double, Texture, List[Position]) = {
      def castRayInternal(xPos:Int, yPos:Int, positions: List[Position]) : (Int, Int, Texture, List[Position]) = {
        val (xCell, yCell) = (xPos/level.cellSize, yPos/level.cellSize)
        println( xPos + ", " + yPos + " " + (xCell, yCell) )
        if( xCell >= level.widthCells || xCell < 0 || yCell >= level.heightCells || yCell < 0 ) {
        	(xPos, yPos, level.wallTexture, positions)
        } else if(level(xCell, yCell).isDefined) {
        	(xPos, yPos, level(xCell, yCell).get, positions)
        } else {
        	castRayInternal(xPos + xStep, yPos + yStep, Position(xPos, yPos) :: positions)
        }
      }
      val (finalXPos, finalYPos, tex, poses) = castRayInternal(xPos, yPos, List())
      (hypot(finalXPos-xPos,finalYPos-yStep), tex, poses)
    }
    
    private[this] var lastDebugInfo : DebugInfo = null
    
    def debugInfo : DebugInfo = lastDebugInfo
    
    def render(pos : Position, angle : Double) : BufferedImage = {
      /*
       * Theta = 0 straight down, angle increases counter clockwise
       * Y increases down
       * X increases right
       */
      
      println( pos + " angle " + toDegrees(angle) )
        
      val columns = for( colNum <- (-outWidth/2) until (outWidth/2) ) yield {
        val angleOffCenter = fov/outWidth * colNum
        val rayAngle = normalizeAngle(angle + angleOffCenter)
        val xDirection = if( rayAngle > 0 ) 1 else if(rayAngle < 0) -1 else 0
        val yDirection = if( abs(rayAngle) > PI/2 ) -1 else if(abs(rayAngle) < PI/2) 1 else 0
        
        val Position(xPos, yPos) = pos
        
        println( pos + " angle " + toDegrees(rayAngle) + " Directions " + xDirection + ", " + yDirection )
        
        // Cast ray that hits virticle walls
        val vRayXStep : Int = level.cellSize*xDirection
        val vRayYStep : Int = (level.cellSize / tan(rayAngle)).toInt
        val vRayXPos:Int = xDirection match {
          case -1 => ((xPos / level.cellSize) * level.cellSize) - 1
          case 0 => 0
          case 1 => ((xPos / level.cellSize) * level.cellSize) + 64
        }
        val vRayYPos:Int = yPos + ( abs(xPos - vRayXPos) / tan(rayAngle) ).toInt * yDirection
        println("V xStep " + vRayXStep + " yStep " + vRayYStep + " x " + vRayXPos + " y " + vRayYPos)
        val (vRayDist, vRayTex, vPoses) = castRay(vRayXPos, vRayYPos, vRayXStep, vRayYStep)

        // Cast ray that hits horiz. walls
        val hRayXStep : Int = (tan(rayAngle) * level.cellSize).toInt
        val hRayYStep : Int = level.cellSize * yDirection
        val hRayYPos:Int = yDirection match {
          case -1 => ((xPos / level.cellSize) * level.cellSize) - 1
          case 0 => 0
          case 1 => ((xPos / level.cellSize) * level.cellSize) + 64
        }
        val hRayXPos:Int = xPos + ( abs(yPos - hRayYPos) * tan(rayAngle) ).toInt * xDirection 
        println("H xStep " + hRayXStep + " yStep " + hRayYStep + " x " + hRayXPos + " y " + hRayYPos)
        val (hRayDist, hRayTex, hPoses) = castRay(hRayXPos, hRayYPos, hRayXStep, hRayYStep)

        val ret = if( hRayDist < vRayDist ) (hRayDist*cos(angleOffCenter), hRayTex) else (vRayDist*cos(angleOffCenter), vRayTex)
        (ret._1, ret._2, RayInfo(rayAngle, vPoses, hPoses))
      }
      
      lastDebugInfo = DebugInfo(pos, angle, (for( (_, _, ray)<- columns ) yield ray).toList)
      
      val img = new BufferedImage(outWidth, outHeight, BufferedImage.TYPE_INT_RGB)
      val g = img.getGraphics.asInstanceOf[Graphics2D]
      g.setColor(java.awt.Color.BLACK)
      g.fillRect(0, 0, outWidth, outHeight)
      g.setStroke(new BasicStroke())
      for( ((dist, tex, _), i) <- columns.toList.zipWithIndex ) {
        val l = (level.cellSize.toDouble/dist * projectionPlaneDistance).toInt
        tex match {
          case Color(r,gr,b) => g.setColor(new java.awt.Color(r.toFloat,gr.toFloat,b.toFloat, 1.0f))
          case _ => g.setColor(java.awt.Color.WHITE)
        }
        println(l + " " + ((dist, tex), i) + " " + g.getColor + " " + (outHeight/2 - l/2, outHeight/2 + l/2))
        g.drawLine(i, outHeight/2 - l/2, i, outHeight/2 + l/2)
      }
      img
    }
    
    def normalizeAngle(th:Double) : Double = if(th > PI) normalizeAngle(th - PI*2) else if(th < -PI) normalizeAngle(th + PI*2) else th  
  }
}
