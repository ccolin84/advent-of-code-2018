import scala.io.Source

object Day3 {
  def main(argv: Array[String]) = {
    val inputFile = "src/input.txt"
    val inputParser = new Day3InputParser(inputFile)
    val squaresWithAtLeastTwoClaims = getSquaresWithAtLeastTwoClaims(inputParser)
    println(s"squares with at least two claims: $squaresWithAtLeastTwoClaims")
  }

  def getSquaresWithAtLeastTwoClaims(inputParser: Day3InputParser): Int = {
    inputParser
      .getClaims
      .foldRight(new FabricSheetGrid())((claim, fabricSheet) => {
        fabricSheet.addSquare(claim.square)
        fabricSheet
      })
      .getNumberOfCoordinatesWithAtLeastTwoClaims
  }
}

class Day3InputParser(inputFile: String) {
  def getClaims(): Iterator[Claim] = {
    Source
      .fromFile(inputFile)
      .getLines
      .map(parseClaim(_))
  }

  /** Takes in a String representation of a fabric sheet square inch
   *  claim and returns a Claim object for that String
   *  ex: "#1 @ 1,3: 4x4"
   */
  def parseClaim(unparsedClaim: String): Claim = {
    val Array(
      unparsedId,
      unparsedLeftTop,
      unparsedWidthHeight) = unparsedClaim.split(" ").filter(_ != "@")
    val id = parseId(unparsedId) 
    val Array(left, top) = parseLeftAndTop(unparsedLeftTop)
    val Array(width, height) = parseWidthAndHeight(unparsedWidthHeight)
    Claim(id, Square(top, left, width, height))
  }

  def parseId(unparsedId: String): Int = unparsedId.filter(_ != '#').toInt

  def parseLeftAndTop(unparsedLeftTop: String): Array[Int] = {
    unparsedLeftTop.filter(_ != ':').split(",").map(_.toInt)
  }

  def parseWidthAndHeight(unparsedWidthHeight: String): Array[Int] = {
    unparsedWidthHeight.split("x").map(_.toInt)
  }
}

object Types {
  type Inch = Int
  type ClaimId = Int
}

case class Coordinate(x: Int, y: Int)

case class Square(
  fromTop: Types.Inch,
  fromLeft: Types.Inch,
  width: Types.Inch,
  height: Types.Inch
)

case class Claim(id: Types.ClaimId, square: Square)

class FabricSheetGrid {
  var coordinateToClaimCount = Map[Coordinate, Int]()

  def addCoordinate(coordinate: Coordinate) = {
    var currentClaimCount = coordinateToClaimCount.getOrElse(coordinate, 0)
    coordinateToClaimCount += (coordinate -> (currentClaimCount + 1))
  }

  /** Adds each coordinate in the square
   *  (increasing Y values go down)
   */
  def addSquare(square: Square) = {
    (square.fromTop + 1)
      .to(square.height + square.fromTop)
      .foreach(yInx => {
        (square.fromLeft + 1)
          .to(square.width + square.fromLeft)
          .foreach(xInx => addCoordinate(Coordinate(xInx, yInx)))
      })
  }

  def getCoordinatesWithAtLeastTwoClaims(): Map[Coordinate, Int] = {
    coordinateToClaimCount.filter({
      case (k, v) => v >= 2
    })
  }

  def getNumberOfCoordinatesWithAtLeastTwoClaims(): Int = {
    getCoordinatesWithAtLeastTwoClaims.size
  }
}

