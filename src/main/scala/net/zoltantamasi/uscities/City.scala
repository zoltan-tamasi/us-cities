package net.zoltantamasi.uscities

import upickle.default.{ReadWriter => RW, macroRW}
import scala.collection.immutable.TreeMap

object City {

  implicit val rw: RW[City] = macroRW

  val GRID_SIZE: Float = scala.util.Properties.envOrElse("GRID_SIZE", "0.03").toFloat
  val CLOSE_CITIES_MINIMUM_COUNT = scala.util.Properties.envOrElse("MIN_CITIES", "5").toInt

  def getCloseCitiesByMap(citiesLatMap: TreeMap[Float, City], citiesLngMap: TreeMap[Float, City], citiesIdMap: Map[Int, City], cityId: Int) = {
    
    var closeCitiesCount = 0
    var iteration = 1
    var closeCitiesIds = List[Int]()

    val city = citiesIdMap.get(cityId).get

    while (closeCitiesCount < CLOSE_CITIES_MINIMUM_COUNT) {
      val lngIds = citiesLngMap
        .range(city.lng - (iteration * GRID_SIZE), city.lng + (iteration * GRID_SIZE))
        .map({ case (_, closeCity) => closeCity.id })
        .filter(id => id != city.id)
        .toList

      val latIds = citiesLatMap
        .range(city.lat - (iteration * GRID_SIZE), city.lat + (iteration * GRID_SIZE))
        .map({ case (_, closeCity) => closeCity.id })
        .filter(id => id != city.id)
        .toList
      
      closeCitiesIds = lngIds.intersect(latIds)
      closeCitiesCount = closeCitiesIds.length
      iteration += 1
    }

    city.copy(closeCities = closeCitiesIds.map(id => citiesIdMap.get(id).get))
  }

  def getCloseCities(latOrdered: List[City], lngOrdered: List[City], city: City) = {
    
    var closeCitiesCount = 0
    var iteration = 1
    var closeCitiesIds = List[Int]()

    while (closeCitiesCount < CLOSE_CITIES_MINIMUM_COUNT) {
      val lngIds = lngOrdered
        .dropWhile(orderedCity => orderedCity.lng <= city.lng - (iteration * GRID_SIZE))
        .takeWhile(orderedCity => orderedCity.lng <= city.lng + (iteration * GRID_SIZE))
        .filter(closeCity => closeCity.id != city.id)
        .map(_.id)
      
      val latIds = latOrdered
        .dropWhile(orderedCity => orderedCity.lat <= city.lat - (iteration * GRID_SIZE))
        .takeWhile(orderedCity => orderedCity.lat <= city.lat + (iteration * GRID_SIZE))
        .filter(closeCity => closeCity.id != city.id)
        .map(_.id)
      
      closeCitiesIds = lngIds.intersect(latIds)
      closeCitiesCount = closeCitiesIds.length
      iteration += 1
    }

    city.copy(closeCities = closeCitiesIds.map(id => latOrdered.find(city => id == city.id).get))
  }

  def calculateDistances(city: City): City = {
    city.copy(
      closeCities = city.closeCities
        .map(closeCity => closeCity.copy(
          distance = Some(haversineDistance(city.lng, city.lat, closeCity.lng, closeCity.lat).toInt)
        ))
    )
  }

  def haversineDistance(longitude1: Double, latitude1: Double, longitude2: Double, latitude2: Double):Double = {
    val R = 6372.8
    val dlat = math.toRadians(latitude2 - latitude1)
    val dlog = math.toRadians(longitude2 - longitude1)
    val a = math.sin(dlat / 2) * math.sin(dlat / 2) + math.cos(math.toRadians(latitude1)) * math.cos(math.toRadians(latitude2)) * math.sin(dlog / 2) * math.sin(dlog / 2)
    val c = 2 * math.atan2(math.sqrt(a), math.sqrt(1 - a))
    val distance = R * c
    return distance
  }
}

case class City(id: Int, name: String, lat: Float, lng: Float, closeCities: List[City] = List(), distance: Option[Int] = None)

object CityLatOrdering extends Ordering[City] {
  def compare(a:City, b:City) = a.lat.compare(b.lat)
}

object CityLngOrdering extends Ordering[City] {
  def compare(a:City, b:City) = a.lng.compare(b.lng)
}
