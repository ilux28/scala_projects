package sample

import fortesting.SparkSessionTestWrapper
import org.apache.spark.sql.{DataFrame, SparkSession}
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types.IntegerType

class ReadCSVFileFromDF {
  def calculateData(dataFrame: DataFrame): DataFrame = {
    val filterLinesInDF = dataFrame.filter(col("PRODUCTLINE") === "Motorcycles" && col("STATUS") === "Shipped")
      .withColumn("SALES", col("SALES").cast(IntegerType))
      .select(col("CITY"), col("SALES"), (col("SALES") / col("PRICEEACH")).as("COUNT"))
      .groupBy("CITY")
      .agg(sum("SALES"), sum("COUNT"))
    filterLinesInDF
  }
}
object ReadCSVFileFromDF extends SparkSessionTestWrapper {
  def main(args: Array[String]): Unit = {
    val logFile = "sales_data_sample.csv"
    /*
    val spark = SparkSession.builder.master("local")
      .appName("SimpleProjectScalaSpark")
      .getOrCreate()
     */
    import spark.implicits._
    val dataFrame: DataFrame = spark.read
      .format("csv")
      .option("header", "true")
      .option("mode", "DROPMALFORMED")
      .load(logFile).cache()
    val readCSVFileFromDF = new ReadCSVFileFromDF
    readCSVFileFromDF.calculateData(dataFrame).show()
    spark.stop()
    /*
    val filterLinesInDF = dataFrame.filter($"PRODUCTLINE" === "Motorcycles" && $"STATUS" === "Shipped")
      .withColumn("SALES", col("SALES").cast(IntegerType))
      .select($"CITY", $"SALES", ($"SALES" / $"PRICEEACH").as("COUNT"))
      .groupBy("CITY")
      .agg(sum("SALES"), sum("COUNT"))
    filterLinesInDF.show()
    spark.stop()
    */
  }
}