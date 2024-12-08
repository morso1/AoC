import scala.io.Source


def readingFile()=
  val file = Source.fromFile("/Users/oivahaapaniemi/IdeaProjects/AoC/src/main/scala/day2_input.txt")
  try
    val lines = file.getLines().toVector
    lines.map {
      _.split(" ").map(_.toInt).toVector
    }
  finally
    file.close()



def isSafe =
  val theFile = this.readingFile()
  var safe = Vector[Vector[Int]]()
  val safeList = Vector(1, 2, 3)
  for lines <- theFile do
    if (lines.sliding(2).forall( x => x(0) < x(1) && safeList.contains(x(1)-x(0)) ))  then
      safe = safe :+ lines
    else if (lines.sliding(2).forall( x => x(0) > x(1) && safeList.contains(x(0)-x(1)) )) then
      safe = safe :+ lines
  safe

def safeSize = isSafe.size

def safeDampener =
  val theFile = this.readingFile().filterNot(isSafe.contains)
  val safeList = Vector(1, 2, 3)


