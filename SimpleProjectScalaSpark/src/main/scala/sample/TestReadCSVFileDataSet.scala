package sample

object TestReadCSVFileDataSet {

  import org.apache.spark.sql.SparkSession
  import org.apache.spark.sql.functions._

  case class InfoData(city: String, numMotorcycles: Int, amount: Double)

  def main(args: Array[String]): Unit = {
    val logFile = "sales_data_sample.csv"
    val spark = SparkSession.builder.master("local").appName("SimpleProjectScalaSpark").getOrCreate()
    import spark.implicits._
    val logData = spark.read.textFile(logFile).cache()
    val filterLinesToDF = logData.filter(line => line.contains("Shipped") && line.contains("Motorcycles"))
      .map { line => (line(17), line(4).toDouble / line(2).toDouble, line(4).toInt) }.toDF("city", "count", "amount")
    filterLinesToDF
      .groupBy("city")
      .agg(sum("count"))
      .agg(sum("amount"))
      .show()
    //val resSum = res.reduce((x, y) => (x._1 + x._2, y._1 + y._2))
    //println(resSum)
    spark.stop()
  }
}