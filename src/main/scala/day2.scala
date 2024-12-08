import scala.io.Source


def readingFile(): Vector[Vector[Int]]=
  val file = Source.fromFile("src/main/files/day2_input")
  try
    val lines = file.getLines().toVector
    lines.map {
      _.split(" ").map(_.toInt).toVector
    }
  finally
    file.close()



def isSafe(input: Vector[Vector[Int]]): Vector[Vector[Int]] =
  var safe = Vector[Vector[Int]]()
  val safeList = Vector(1, 2, 3)
  for lines <- input do
    if lines.sliding(2).forall( x => x(0) < x(1) && safeList.contains(x(1)-x(0)) )  then
      safe = safe :+ lines
    else if lines.sliding(2).forall( x => x(0) > x(1) && safeList.contains(x(0)-x(1)) ) then
      safe = safe :+ lines
  safe



def newSafe(lines: Vector[Int]): Boolean =
  val safeList = Vector(1, 2, 3)
  if lines.sliding(2).forall(x => x(0) < x(1) && safeList.contains(x(1) - x(0))) then
    true
  else if lines.sliding(2).forall(x => x(0) > x(1) && safeList.contains(x(0) - x(1))) then
    true
  else
    false


def dampForAll(file: Vector[Vector[Int]]) =
  var number = 0
  for lines <- file do
    if safeDampener(lines) then
      number += 1
  def safeDampener(lines: Vector[Int]): Boolean =
    var boolean = false
    for i <- lines.indices do
      if this.newSafe(lines.slice(0, i) ++ lines.slice(i+1, lines.size)) then
        boolean = true
    boolean
  number

def size = dampForAll(readingFile())
