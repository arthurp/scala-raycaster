package org.singingwizard.raycasting.test

object Test1 {
  import engine.RayCaster._
  import java.lang.Math
  import java.io.File
  import javax.imageio.ImageIO

  
  def main(args : Array[String]) : Unit = {
	val b = new LevelBuilder(10, 10)
	b(0,0) = new Color(0,1,1)
	println(b(0,0))
 
	val l = new Level(b)
	val r = new Renderer(l, Math.toRadians(60), 800, 600)
	
	val img = r.render(Position(130, 130), Math.PI/4)

	ImageIO.getWriterFormatNames.foreach( println _ )
 
	ImageIO.write(img, "JPEG", new File("test.jpg"))
  }
}
