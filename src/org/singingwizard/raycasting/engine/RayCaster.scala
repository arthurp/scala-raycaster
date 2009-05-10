package org.singingwizard.raycasting.engine

import java.awt.image.BufferedImage
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
  
  class Renderer(val level : Level, val fov:Double, val outWidth:Int, val outHeight:Int) {
    import java.lang.Math._
      
    private[this] def castRay(xPos:Int, yPos:Int, xStep:Int, yStep:Int) : (Double, Texture) = {
      def castRayInternal(xPos:Int, yPos:Int) : (Int, Int, Texture) = {
        val (xCell, yCell) = (xPos/level.cellSize, yPos/level.cellSize)
        println( xPos + ", " + yPos )
        if( xCell >= level.widthCells || xCell < 0 || yCell >= level.heightCells || yCell < 0 ) {
        	(xPos, yPos, level.wallTexture)
        } else if(level(xCell, yCell).isDefined) {
        	(xPos, yPos, level(xCell, yCell).get)
        } else {
        	castRayInternal(xPos + xStep, yPos + yStep)
        }
      }
      val (finalXPos, finalYPos, tex) = castRayInternal(xPos, yPos)
      (hypot(finalXPos-xPos,finalYPos-yStep), tex)
    }
    
    def render(pos : Position, angle : Double) : BufferedImage = {
      println( pos + " angle " + toDegrees(angle) )
        
      val columns = for( colNum <- (-outWidth/2) until (outWidth/2) ) yield {
        val rayAngle = normalizeAngle(angle + (fov/outWidth * colNum))
        val xDirection = if( rayAngle > 0 ) 1 else if(rayAngle < 0) -1 else 0
        val yDirection = if( abs(rayAngle) > PI/2 ) 1 else if(abs(rayAngle) < PI/2) -1 else 0
        
        val Position(xPos, yPos) = pos
        
        println( pos + " angle " + toDegrees(rayAngle) + " Directions " + xDirection + ", " + yDirection )
        
        val xRayXStep : Int = level.cellSize*xDirection
        val xRayYStep : Int = (tan(rayAngle) * level.cellSize).toInt
        val initialStepRatio = try { xDirection match {
          case -1 => level.cellSize / (xPos - (((xPos:Double) / level.cellSize) * level.cellSize))
          case 0 => 0.0
          case 1 => level.cellSize / (xPos - (((xPos:Double) / level.cellSize + 1) * level.cellSize))
        } } catch { case e : java.lang.ArithmeticException => 0.0 }
        println("xStep " + xRayXStep + " yStep " + xRayYStep + " initialStepRatio " + initialStepRatio)
        val (xRayDist, xRayTex) = castRay(xPos + (xRayXStep * initialStepRatio).toInt, yPos + (xRayYStep * initialStepRatio).toInt, xRayXStep, xRayYStep)

        /*val yRayXStep = (tan(rayAngle) * level.cellSize).asInstanceOf[Int]
        val yRayYStep = level.cellSize*yDirection
        val (yRayDist, yRayTex) = castRay(0, 0, yRayXStep, yRayYStep)*/
        val ret = (xRayDist, xRayTex)
        println(ret)
        ret
      }
      
      println(columns)
      
      val img = new BufferedImage(outWidth, outHeight, BufferedImage.TYPE_4BYTE_ABGR)
      val g = img.getGraphics
      g.setColor(java.awt.Color.BLACK)
      g.fillRect(0, 0, outWidth, outHeight)
      img
    }
    
    def normalizeAngle(th:Double) : Double = if(th > PI) normalizeAngle(th - PI*2) else if(th < -PI) normalizeAngle(th + PI*2) else th  
  }
}
