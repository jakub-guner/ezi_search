package main.scala

import weka.clusterers.SimpleKMeans
import java.util.ArrayList
import weka.core.Attribute
import weka.core.Instances
import weka.core.Instance
import weka.core.DenseInstance
/**
  * Created by tomasz on 17.11.16.
  */
object KMEANS {
  def computeClusters(data:List[List[Double]]) = {
    val N = data.length
    val M = data(0).length

    val kmeans = new SimpleKMeans()

    kmeans.setSeed(5);
    kmeans.setPreserveInstancesOrder(true)
    kmeans.setNumClusters(9)

    var attributes = new ArrayList[Attribute]()
    for (i <- 1 to M) {
      attributes.add(new Attribute(i.toString))
    }

    val dataInstances = new Instances("ASD", attributes, 100)

    // Create the instance
    for (i <- 0 to N - 1) {
      dataInstances.add(new DenseInstance(1.0, data(i).toArray))
    }

    kmeans.buildClusterer(dataInstances)

    kmeans.getAssignments
  }
}
