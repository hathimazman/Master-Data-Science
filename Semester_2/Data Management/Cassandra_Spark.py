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