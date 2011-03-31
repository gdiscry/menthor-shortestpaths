package processing.parallel

import math.min

class ShortestPathVertex(label: String) extends
    Vertex[Double](label, Double.PositiveInfinity) {
  private var weights: Map[Vertex[Double],Double] = Map()

  val isSource = false

  override def update() = {
    val mindist = if (isSource) 0.0 else incoming.map(msg => msg.value).min
    if (mindist < value) {
      value = mindist
      for (neighbor <- neighbors) yield
        Message(this, neighbor, mindist + weights(neighbor))
    } else
      List()
  }

  def connectTo(weight: Double)(v: Vertex[Double]) {
    if (weight < weights.getOrElse(v,Double.PositiveInfinity))
      weights = weights + (v -> weight)
    super.connectTo(v)
  }

  override def connectTo(v: Vertex[Double]) {
    this.connectTo(1.0)(v)
  }
}
