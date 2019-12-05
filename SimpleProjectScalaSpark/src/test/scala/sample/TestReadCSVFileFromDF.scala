package sample

import fortesting.SparkSessionTestWrapper
import org.scalatest.FunSuite
import org.apache.spark.sql.{DataFrame, Row}
import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.sql.functions._

  class TestReadCSVFileFromDF
  extends FunSuite
  with SparkSessionTestWrapper {
  test("Check method for manipulating of data from dataFrame") {
    val conf = new SparkConf().setAppName("SimpleProjectScalaSpark").setMaster("local")
    val sc = new SparkContext(conf)
    import org.apache.spark.sql.types._
    val schema = StructType(List(
      StructField("PRODUCTLINE", StringType, nullable = true),
      StructField("STATUS", StringType, nullable = true),
      StructField("CITY", StringType, nullable = true),
      StructField("SALES", IntegerType, nullable = true),
      StructField("PRICEEACH", IntegerType, nullable = true)
    ))
    val rdd = sc.parallelize(Seq(
      Row("Motorcycles", "Shipped", "qwerty", 200, 2)
    ))
    val sqlContext = spark.sqlContext
    val df = sqlContext.createDataFrame(rdd, schema)
    val instanceReadCSVFileFromDF = new ReadCSVFileFromDF
    val dfResult = instanceReadCSVFileFromDF.calculateData(df)
    //println(dfResult.select(col("sum(COUNT)").alias("sum(COUNT)")))
    val res: Array[Row] = dfResult.select(col("sum(COUNT)").alias("sum(COUNT)")).collect()
    assert(res(0)(0) == 100)
    spark.stop()
  }
}