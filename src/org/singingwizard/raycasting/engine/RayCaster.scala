package org.singingwizard.raycasting.engine

import java.awt.image.BufferedImage
import java.awt.Graphics2D
import java.awt.BasicStroke
//import java._

object RayCaster {
  abstract class Texture extends NotNull {
    def shade(amount:Double) : Texture 
  }
  case class Color(red:Double, green:Double, blue:Double) extends Texture {
    def shade(a:Double) = Color(red*a,green*a,blue*a)
  }
  
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
    
    class LevelFiller(b:LevelBuilder, x:Int, y:Int) {
      def |(tex : Texture) : LevelFiller = {
        b(x,y) = tex
        new LevelFiller(b, x+1, y)
      }
      def |(i : Int) : LevelFiller = {
        new LevelFiller(b, x+1, y)
      }
      def |/(tex : Texture) : LevelFiller = {
        b(x,y) = tex
        new LevelFiller(b, 0, y+1)
      }
      def |/(i : Int) : LevelFiller = {
        new LevelFiller(b, 0, y+1)
      }
    }
    
    def <<(tex : Texture) : LevelFiller = {
      this(0,0) = tex
      new LevelFiller(this, 1, 0)
    }
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
    
    private[this] def roundToNearest(v:Double) : Int = {
      floor(v+1).toInt
    }
    
    private[this] def castRay(xPos:Double, yPos:Double, xStep:Double, yStep:Double) : (Double, Double, Texture, List[Position]) = {
      def castRayInternal(xPos:Double, yPos:Double, positions: List[Position]) : (Double, Double, Texture, List[Position]) = {
        val (xCell, yCell) = (floor(floor(xPos)/level.cellSize).toInt, floor(floor(yPos)/level.cellSize).toInt)
        debugPrint( xPos + ", " + yPos + " " + (xCell, yCell) )
        if( xCell >= level.widthCells || xCell < 0 || yCell >= level.heightCells || yCell < 0 ) {
        	(xPos, yPos, level.wallTexture, Position(xPos.toInt, yPos.toInt) :: positions)
        } else if(level(xCell, yCell).isDefined) {
        	(xPos, yPos, level(xCell, yCell).get, Position(xPos.toInt, yPos.toInt) :: positions)
        } else {
        	castRayInternal(xPos + xStep, yPos + yStep, Position(xPos.toInt, yPos.toInt) :: positions)
        }
      }
      
      if( xStep == 0 && yStep == 0 ) {
        (Math.MAX_INT, Math.MAX_INT, level.wallTexture, Nil)
      } else {
    	  castRayInternal(xPos, yPos, List())
    	  //(finalXPos,finalYPos, tex, poses)
      }
    }
    
    private[this] var lastDebugInfo : DebugInfo = null
    
    def debugInfo : DebugInfo = lastDebugInfo
    
    val EPSILON = .00001

    
    def render(pos : Position, angle : Double) : BufferedImage = {
      /*
       * Theta = 0 due right, angle increases clockwise
       * Y increases down
       * X increases right
       */
      
      debugPrint( pos + " angle " + toDegrees(angle) )
        
      val columns = for( colNum <- (-outWidth/2) until (outWidth/2) ) yield {
        val angleOffCenter = fov/outWidth * colNum
        val rayAngle = normalizeAngle(angle + angleOffCenter)
        val yDirection = if( rayAngle > 0 ) 1 else if(rayAngle < 0) -1 else 0
        val xDirection = if( abs(rayAngle) > PI/2 ) -1 else if(abs(rayAngle) < PI/2) 1 else 0
        
        val Position(xPos, yPos) = pos
        
        debugPrint( pos + " angle " + toDegrees(rayAngle) + " Directions " + xDirection + ", " + yDirection )

        val tanRayAng = tan(rayAngle)
        
        // Cast ray that hits virticle walls
        val vRayTan = abs(tan(rayAngle))*yDirection
        val vRayXStep = level.cellSize*xDirection
        val vRayYStep = level.cellSize * vRayTan
        val vRayXPos = xDirection match {
          case -1 => ((xPos / level.cellSize) * level.cellSize) - EPSILON
          case 0 => 0.
          case 1 => ((xPos / level.cellSize) * level.cellSize) + level.cellSize + EPSILON
        }
        val vRayYPos = yPos + abs(xPos - vRayXPos) * vRayTan
        debugPrint("V xStep " + vRayXStep + " yStep " + vRayYStep + " x " + vRayXPos + " y " + vRayYPos)
        val (vRayX, vRayY, vRayTex, vPoses) = castRay(vRayXPos, vRayYPos, vRayXStep, vRayYStep)
        val vRayDist = hypot(vRayX-xPos, vRayY-yPos)

        // Cast ray that hits horiz. walls
        val hRayTan = abs(tan(rayAngle))*xDirection
        val hRayXStep = level.cellSize / hRayTan
        val hRayYStep = level.cellSize * yDirection
        val hRayYPos = yDirection match {
          case -1 => ((yPos / level.cellSize) * level.cellSize) - EPSILON
          case 0 => 0.
          case 1 => ((yPos / level.cellSize) * level.cellSize) + level.cellSize + EPSILON
        }
        val hRayXPos = xPos + abs(yPos - hRayYPos) / hRayTan 
        debugPrint("H xStep " + hRayXStep + " yStep " + hRayYStep + " x " + hRayXPos + " y " + hRayYPos)
        val (hRayX, hRayY, hRayTex, hPoses) = castRay(hRayXPos, hRayYPos, hRayXStep, hRayYStep)
        val hRayDist = hypot(hRayX-xPos, hRayY-yPos)

        //*cos(angleOffCenter)
        val (retDist, retTexture) = if( hRayDist < vRayDist ) 
        			(hRayDist*cos(angleOffCenter), hRayTex.shade(0.6)) 
        		  else 
        			(vRayDist*cos(angleOffCenter), vRayTex.shade(if(xDirection>0) 1 else 0.9))
        (retDist, retTexture, RayInfo(rayAngle, vPoses.reverse, hPoses.reverse))
      }
      
      lastDebugInfo = DebugInfo(pos, angle, (for( (_, _, ray)<- columns ) yield ray).toList)
      
      val img = new BufferedImage(outWidth, outHeight, BufferedImage.TYPE_INT_RGB)
      val g = img.getGraphics.asInstanceOf[Graphics2D]
      g.setColor(java.awt.Color.BLACK)
      g.fillRect(0, 0, outWidth, outHeight)
      g.setStroke(new BasicStroke())
      for( ((dist, tex, _), i) <- columns.toList.zipWithIndex ) {
        val l = (level.cellSize.toDouble/dist * projectionPlaneDistance).toInt
        g.setColor(tex match {
          case Color(r,gr,b) => new java.awt.Color(r.toFloat,gr.toFloat,b.toFloat, 1.0f)
          case _ => java.awt.Color.WHITE
        })
        debugPrint(l + " " + ((dist, tex), i) + " " + g.getColor + " " + (outHeight/2 - l/2, outHeight/2 + l/2))
        g.drawLine(i, outHeight/2 - l/2, i, outHeight/2 + l/2)
      }
      img
    }
    
    def normalizeAngle(th:Double) : Double = if(th > PI) normalizeAngle(th - PI*2) else if(th < -PI) normalizeAngle(th + PI*2) else th  
  }
  
  private[this] def debugPrint( str : => String ) {}
}
