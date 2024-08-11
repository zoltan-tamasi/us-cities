package net.zoltantamasi.uscities

import scala.io.Source
import scala.util.{Try, Success, Failure}
import upickle.default._
import scala.collection.immutable.TreeMap

object UsCities {

  val filename = "uscities.csv"
  val delimiter = ","

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
    
    var cities: List[City] = List[City]()

    var citiesIdMap: Map[Int, City] = Map[Int, City]()
    var citiesLatMap: TreeMap[Float, City] = TreeMap[Float, City]()
    var citiesLngMap: TreeMap[Float, City] = TreeMap[Float, City]()

    for (line <- file.getLines().drop(1)) {
     
      val city = readCity(line);

      city match {
        case Success(city) =>

          citiesIdMap = citiesIdMap.updated(city.id, city)
          citiesLatMap = citiesLatMap.updated(city.lat, city)
          citiesLngMap = citiesLngMap.updated(city.lng, city)
    
          cities = cities :+ city

        case Failure(error) =>
          println(s"Error reading line: $line \n Error is $error")
      }
    }

    //val lngOrdered = cities.sorted(CityLngOrdering)
    //val latOrdered = cities.sorted(CityLatOrdering)

    val t3 = System.currentTimeMillis()

    val closeCities = 
      cities
        //.map(City.getCloseCities(latOrdered, lngOrdered, _))
        .map(city => City.getCloseCitiesByMap(citiesLatMap, citiesLngMap, citiesIdMap, city.id))
        .map(City.calculateDistances(_))

    val t4 = System.currentTimeMillis()
    val filterTime = t4 - t3

    
    file.close()
    
    println(s"filter time: $filterTime")

    os.write.over(os.pwd / "us-close-cities.json", upickle.default.write(closeCities))

  }
}

