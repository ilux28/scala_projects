package fortesting

import org.apache.spark.sql.DataFrame
import org.apache.spark.sql.functions._

object HelloWord {
  def withGreeting()(df: DataFrame): DataFrame = {
    df.withColumn("greeting", lit("hello word"))
  }
}
