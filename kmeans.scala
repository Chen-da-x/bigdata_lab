import scala.io.Source
import scala.util.Random

object KMeansClustering {
  // Helper functions
  def distance(p: Vector[Double], q: Vector[Double]): Double = {
    math.sqrt(p.zip(q).map { case (pi, qi) => math.pow(pi - qi, 2) }.sum)
  }

  def closestPoint(q: Vector[Double], candidates: Array[Vector[Double]]): Vector[Double] = {
    candidates.minBy(candidate => distance(q, candidate))
  }

  def addVec(v1: Vector[Double], v2: Vector[Double]): Vector[Double] = {
    v1.zip(v2).map { case (x1, x2) => x1 + x2 }
  }

  def average(cluster: Iterable[Vector[Double]]): Vector[Double] = {
    val sumVector = cluster.reduce(addVec)
    val count = cluster.size
    sumVector.map(_ / count)
  }

  def readDataPointsFromFile(filename: String): Array[Vector[Double]] = {
    Source.fromFile(filename)
      .getLines()
      .map(_.split("\t").map(_.toDouble).toVector)
      .toArray
  }

  def chooseRandomCentroids(data: Array[Vector[Double]], k: Int): Array[Vector[Double]] = {
    Random.shuffle(data.toList).take(k).toArray
  }

  // Main program
  def main(args: Array[String]): Unit = {
    val data = readDataPointsFromFile("clustering_dataset.txt")
    var centroids = chooseRandomCentroids(data, 3)
    var prevCentroids = Array[Vector[Double]]()
    val maxIterations = 100
    var hasConverged = false
    val epsilon = 0.001

    val writer = new PrintWriter(new File("centroids_output.txt"))

    for (iteration <- 1 to maxIterations if !hasConverged) {
      val clusters = data.groupBy(closestPoint(_, centroids))
      prevCentroids = centroids
      centroids = clusters.map { case (_, vectors) => average(vectors) }.toArray

      // Check for convergence
      hasConverged = centroids.zip(prevCentroids).forall {
        case (newC, oldC) => distance(newC, oldC) < epsilon
      }

      if (hasConverged) {
        writer.println(s"Converged at iteration $iteration")
      }
    }

    // Output the results to a file
    centroids.foreach(centroid => writer.println(s"Centroid: ${centroid.mkString(", ")}"))
    writer.close()
  }
}
