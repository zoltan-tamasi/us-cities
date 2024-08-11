package net.zoltantamasi.uscities

import scala.io.Source
import upickle.default._
import scala.collection.immutable.TreeMap

object UsCities {
  def main(args: Array[String]): Unit = {

    val filename = "uscities.csv"
    val delimiter = ","
    val file = Source.fromFile(filename)
    
    var wrongLines: List[String] = List[String]()
    
    var cities: List[City] = List[City]()

    var citiesIdMap: Map[Int, City] = Map[Int, City]()
    var citiesLatMap: TreeMap[Float, City] = TreeMap[Float, City]()
    var citiesLngMap: TreeMap[Float, City] = TreeMap[Float, City]()


    for (line <- file.getLines().drop(1)) {
      val fields = line.split(delimiter).map(_.trim).map(_.replace("\"", ""))
      
      try {
        val name = fields(1)
        val lat = fields(6).toFloat
        val lng = fields(7).toFloat
        val id = fields(16).toInt

        val city = City(id.toInt, name, lat.toFloat, lng.toFloat)

        citiesIdMap = citiesIdMap.updated(id.toInt, city)
        citiesLatMap = citiesLatMap.updated(lat, city)
        citiesLngMap = citiesLngMap.updated(lng, city)

        cities = cities :+ City(id.toInt, name, lat.toFloat, lng.toFloat)
      } catch {
        case _: Exception => {
          wrongLines = wrongLines :+ line
        }
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

    wrongLines.foreach(s => println(s"wrong line: $s"))
    file.close()
    
    println(s"filter time: $filterTime")

    os.write.over(os.pwd / "us-close-cities.json", upickle.default.write(closeCities))

  }
}

