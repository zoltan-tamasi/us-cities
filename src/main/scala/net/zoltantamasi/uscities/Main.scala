package net.zoltantamasi.uscities

import scala.io.Source
import scala.util.{Try, Success, Failure}
import upickle.default._
import scala.collection.immutable.TreeMap

object UsCities {

  val filename = "uscities.csv"
  val delimiter = ","
  val ALGO_TYPE = scala.util.Properties.envOrElse("ALGO", "TREE").trim

  if (!ALGO_TYPE.equals("LIST") && !ALGO_TYPE.equals("TREE")) {
    println(s"Value of ALGO must be TREE or LIST, but was: $ALGO_TYPE");
    System.exit(0)
  }

  def readCity(line: String): Try[City] = {
    val fields = line.split(delimiter).map(_.trim).map(_.replace("\"", ""))
      
    Try {

      val name = fields(1)
      val lat = fields(6).toFloat
      val lng = fields(7).toFloat
      val id = fields(16).toInt

      City(id.toInt, name, lat.toFloat, lng.toFloat)
    }
  }

  def main(args: Array[String]): Unit = {
    
    val file = Source.fromFile(filename)

    var citiesIdMap: Map[Int, City] = Map[Int, City]()
    var citiesLatMap: TreeMap[Float, City] = TreeMap[Float, City]()
    var citiesLngMap: TreeMap[Float, City] = TreeMap[Float, City]()

    val cities: List[City] = 
      file
        .getLines()
        .drop(1)
        .map(line => {
          val city = readCity(line);
      
          city match {
            case Success(city) =>
      
              citiesIdMap = citiesIdMap.updated(city.id, city)
              citiesLatMap = citiesLatMap.updated(city.lat, city)
              citiesLngMap = citiesLngMap.updated(city.lng, city)
        
              Some(city)
      
            case Failure(error) =>
              println(s"Error reading line: $line \n Error is $error")
              None
          }
        })
        .toList
        .flatten

    file.close()
        
    val start = System.currentTimeMillis()

    val closeCities = ALGO_TYPE match {
      case "LIST" =>

        val lngOrdered = cities.sorted(CityLngOrdering)
        val latOrdered = cities.sorted(CityLatOrdering)
        
        cities
          .map(City.getCloseCities(latOrdered, lngOrdered, _))
          .map(City.calculateDistances(_))

      case "TREE" => 
        cities
          .map(city => City.getCloseCitiesByMap(citiesLatMap, citiesLngMap, citiesIdMap, city.id))
          .map(City.calculateDistances(_))
    }

    val end = System.currentTimeMillis()
    val time = end - start
    val count = cities.length
    
    println(s"$count cities processed with algo: $ALGO_TYPE in: $time ms")

    os.write.over(os.pwd / "us-close-cities.json", upickle.default.write(closeCities))
  }
}

