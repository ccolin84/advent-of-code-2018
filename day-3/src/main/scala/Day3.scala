import scala.io.Source

object Day3 {
  def main(argv: Array[String]) = {
    val inputFile = "src/input.txt"
    val inputParser = Day3InputParser(inputFile)
    val squaresWithAtLeastTwoClaims = getSquaresWithAtLeastTwoClaims(inputClaims)
  }

  def getSquaresWithAtLeastTwoClaims(inputParser: Day3InputParser) {
    val fabricSheet = new FabricSheet()
    inputParser.mapClaims[Unit]()
  }
}

class Day3InputParser(inputFile: string) {
  def mapClaims[A](fn: Claim => A): Iterator[Claim] = {
    Source
      .fromFile(inputFile)
      .readLines
      .map(parseClaim)
  }

  def parseClaim(claim: String): Claim  {
  }
}

object Types {
  type Inch = Int
  type ClaimId = Int
}

class FabricSheet {
  var coordinateToClaimCount = Map[Types.Coordinate, Int]()

  def addClaim(coordinate: Types.Coordinate) = {
    var currentClaimCount = coordinateToClaimCount.getOrElse(coordinate, 0)
    coordinateToClaimCount += (coordinate -> currentClaimCount + 1)
  }

  def getCoordinatesWithAtLeastTwoClaims() = {
    coordinateToClaimCount.foldRight(0)()
  }
}

class FabricSquare(coordinate: Coordinate) {
  val coordinate = coordinate
}

case class Coordinate(
  fromTop: Types.Inch,
  fromLeft: Types.Inch,
  x: Types.Inch,
  y: Types.Inch
)

case class Claim(id: Types.ClaimId, coordinate: Types.Coordinate)
