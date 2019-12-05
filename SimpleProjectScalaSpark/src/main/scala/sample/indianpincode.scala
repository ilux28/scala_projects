package sample

import java.io.StringReader

import au.com.bytecode.opencsv.CSVReader
import org.apache.spark.sql.SparkSession

object indianpincode {

  case class Info(cities: String, numMotorcycles: String, totalCount: String)

  def main(args: Array[String]): Unit = {
    val logFile = "sales_data_sample.csv"
    val spark = SparkSession.builder.master("local").appName("SimpleProjectScalaSpark").getOrCreate()
    import spark.implicits._
    val logData = spark.read.textFile(logFile).cache()
    val filterLines = logData.filter(line => line.contains("Shipped") && line.contains("Motorcycles"))
    val massPrice = filterLines.map( line => line.split(","))
    val result = filterLines.map{ line =>
      val reader = new CSVReader(new StringReader(line))
      reader.readNext()
    }
    val res = massPrice.map( x => (x(4).toDouble / x(2).toDouble, x(4).toDouble ))
    val resSum = res.reduce((x, y) => (x._1 + x._2, y._1 + y._2))
    println(resSum)
    spark.stop()
  }
}