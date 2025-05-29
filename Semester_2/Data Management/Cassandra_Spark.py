import os
import sys
from pyspark.sql import SparkSession, Row

# Windows compatibility fix
os.environ['PYSPARK_PYTHON'] = sys.executable
os.environ['PYSPARK_DRIVER_PYTHON'] = sys.executable

def parseInput(line):
    fields = line.split('|')
    return Row(user_id=int(fields[0]), age=int(fields[1]), 
               gender=fields[2], occupation=fields[3], zip=fields[4])

# Create SparkSession with Cassandra connector
spark = SparkSession.builder \
    .appName("CassandraIntegration") \
    .master("local[*]") \
    .config("spark.jars.packages", "com.datastax.spark:spark-cassandra-connector_2.11:2.3.0") \
    .config("spark.cassandra.connection.host", "192.168.56.101") \
    .config("spark.cassandra.connection.port", "9042") \
    .config("spark.driver.host", "127.0.0.1") \
    .config("spark.driver.bindAddress", "127.0.0.1") \
    .getOrCreate()

# Test connection
print(f"Spark Version: {spark.version}")
print("Cassandra connector loaded successfully!")



from pyspark.sql import SparkSession
from pyspark.sql import Row
from pyspark.sql import functions

def parseInput(line):
    fields = line.split('|')
    return Row(user_id=int(fields[0]), age=int(fields[1]), gender=fields[2], occupation=fields[3], zip=fields[4])

if __name__ == "__main__":
   # Create SparkSession
   spark = Spark.session.builder \
        .appName("CassandraIntegration") \
        .config("spark.cassandra.connection.host", "127.0.0.1") \
        .getOrCreate()
   
   # Get the raw data
   lines = spark.SparkContext.textFile("hdfs:///user/maria_dev/hathim/ml-100k/u.user")
   
   # conver to RDD of Row objects
   usersDataset = lines.map(parseInput)

   # Convert to DataFrame
   usersDataset.write \
        .format("org.apache.spark.sql.cassandra") \
        .mode("append") \
        .options(table="users", keyspace="movielens") \
        .save()
   
   # Read it back from Cassandra into new dataframe
   readUsers = spark.read \
        .format("org.apache.spark.sql.cassandra") \
        .options(table="users", keyspace="movielens") \
        .load()
   
   readUsers.createOrReplaceTempView("users")

   sqlDF = spark.sql("SELECT * FROM users WHERE age > 20")
   sqlDF.show()

   spark.stop()