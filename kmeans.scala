import scala.io.Source
import scala.util.Random

def distance(p:Vector[Double], q:Vector[Double]) : Double = {
    math.sqrt(p.zip(q).map { case (pi, qi) => math.pow(pi-qi, 2)}.sum)
}
 
def clostestpoint(q: Vector[Double], candidates: Array[Vector[Double]]): Vector[Double] = {
	candidates.minBy(candidate => distance(q, candidate))
}
 
def add_vec(v1: Vector[Double], v2: Vector[Double]): Vector[Double] = {
	v1.zip(v2).map { case (x1, x2) => x1 + x2 }
}
 
def average(cluster: Iterable[Vector[Double]]): Vector[Double] = {
    val sumVector = cluster.reduce(add_vec)
    val count = cluster.size
    sumVector.map(_ / count)
}

def readDataPointsFromFile(filename: String): Array[Vector[Double]] = {
  Source.fromFile(filename)
    .getLines()
    .map { line =>
      line.split("\t").map(_.toDouble).toVector
    }.toArray
}

def choose_random_centroids(data: Array[Vector[Double]], k: Int): Array[Vector[Double]] = {
  Random.shuffle(data.toList).take(k).toArray
}

////////////////////////
// Main Program Below //
////////////////////////

object KMeansClustering {
  def main(args: Array[String]): Unit = {
    // Sample data: each vector is a point in the dataset
    val data = readDataPointsFromFile("clustering_dataset.txt")

    // Initial centroids: randomly chosen or predefined
    var centroids = choose_random_centroids(data, 3)
    var prevCentroids = Array[Vector[Double]]()
    val maxIterations = 100
    var hasConverged = false
    val epsilon = 0.001 

    for (iteration <- 1 to maxIterations if !hasConverged) {
      val clusters = data.groupBy(clostestpoint(_, centroids))
      prevCentroids = centroids
      centroids = clusters.map { case (_, vectors) => average(vectors) }.toArray

      // Check for convergence
      hasConverged = centroids.zip(prevCentroids).forall {
        case (newC, oldC) => distance(newC, oldC) < epsilon
      }

      if (hasConverged) {
        println(s"Converged at iteration $iteration")
      }
    }

    // Output the results
    centroids.foreach(centroid => println(s"Centroid: ${centroid.mkString(", ")}"))
  }
}

//Write something here.