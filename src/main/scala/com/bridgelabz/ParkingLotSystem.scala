package com.bridgelabz

import java.text.SimpleDateFormat
import java.util.Calendar
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer
// Parking Lot class to handle park and unPark
class ParkingLotSystem(parkingLotCapacity:Int,parkingLot:Int = 0)
{
  // ListBuffer to store vehicles in parking lot
  var vehicles = new ListBuffer[Vehicle]
  var observers = new ListBuffer[ParkingLotObserver]
  var totalCapacity = parkingLotCapacity
  var parkingLotMap :Map[Int,Int] = Map(0->0)
  var timingOfParking = ""
  var lotSize = 0
  var parkingLotArray = Array.ofDim[Vehicle](parkingLot,totalCapacity)
  /**
   *
   * @param capacity : Set total capacity of parking lot
   */
  def setCapacity(capacity: Int): Unit ={
    this.totalCapacity = capacity
  }

  /**
   *
   * @param observer: To register as an observer
   */
  def registerParkingLotObserver(observer: ParkingLotObserver) = {
    this.observers += observer
  }

  def parkAtSlot(slotNumber: Int): Boolean = {
    if(slotNumber > totalCapacity)  false
    else if(this.parkingLotArray.size >= slotNumber) false
    else true
  }

  /**
   *
   * @param vehicle : vehicle to park
   * @param driverType : normal or handicap driver
   * @param vehicleType : large or normal vehicle
   * @throws com.bridgelabz.ParkingLotException : Exception for vehicle already parked or parking lot full
   * @return
   */
    @throws(classOf[ParkingLotException])
  def park(vehicle : Vehicle,driverType: String,vehicleType:String*): Boolean =
    {
      try {
        getTimeOfPark()
        if (isVehicleParked(vehicle)) {
          throw new ParkingLotException("Vehicle already parked")
        }
        if(parkingLotArray.size == parkingLot*totalCapacity)
        {
          observers.foreach(observer => observer.capacityIsFull())
          throw new ParkingLotException("Parking Lot Full")
        }
        if(parkingLot == 0){
          for(capacity <- 0 to totalCapacity-1)
          {
            if(parkingLotArray(capacity) != null) {
              parkingLotArray(0)(capacity) = vehicle
              return true
            }
          }
          return false
        }
        else {
          if(driverType.equals("normal"))
          {
            val status = normalDriverType(vehicle,vehicleType(0))
            return status
          }
          else {
            for (lot <- 0 to parkingLot - 1) {
              for (capacity <- 0 to totalCapacity - 1) {
                if (parkingLotArray(lot)(capacity) == null) {
                  parkingLotArray(lot)(capacity) = vehicle
                  return true
                }
              }
            }
          }
          return true
          }
        false
      }
        catch {
          case _: ParkingLotException => {
            println("Parking Lot Full")
            false
          }
          case ex: Exception => {
            println(ex.getMessage())
            false
          }
        }
  }

  /**
   *
   * @param vehicle : Vehicle to park
   * @param vehicleType : Large or normal vehicle
   * @return
   */
  def normalDriverType(vehicle : Vehicle,vehicleType:String) : Boolean = {
    for(lot <- 0 to parkingLot-1){
      for(capacity <- 0 to totalCapacity-1){
        if(vehicleType.equals("Large"))
        {
          var lotNumber = -1
          if(parkingLotMap.size != 0){
            parkingLotMap.foreach(value => {
              if(lot == value._1)
              {
                if(value._2 < totalCapacity){
                  lotNumber = lot
                }
              }
            })
            if(parkingLotArray(lotNumber)(capacity)==null){
              parkingLotArray(lotNumber)(capacity) = vehicle
              parkingLotMap.foreach(i => {
                lotSize = i._2
                lotSize += 1
                parkingLotMap += (lotNumber -> lotSize)
              })
              return true
            }
          }

        }
        else
        {
          if(parkingLotArray(lot)(capacity)==null){
            parkingLotArray(lot)(capacity) = vehicle
            parkingLotMap.foreach(i => {
              lotSize = i._2
              lotSize += 1
              parkingLotMap += (lot -> lotSize)
            })
            return true
          }
          else{
            if(lot+1 <= parkingLot-1){
              if(parkingLotArray(lot+1)(capacity)==null){
                parkingLotArray(lot)(capacity) = vehicle
                parkingLotMap.foreach(i => {
                  lotSize = i._2
                  lotSize += 1
                  parkingLotMap += (lot -> lotSize)
                })
                return true
              }
            }
          }
        }
      }
    }
    false
  }
  def getTimeOfPark() : String = {
    val calender = Calendar.getInstance();
    val hour = new SimpleDateFormat("hh");
    val hours = hour.format(calender.getTime());
    val min = new SimpleDateFormat("mm");
    val minutes = min.format(calender.getTime());
    timingOfParking = hour + "" + minutes
    timingOfParking
  }
  /**
   *
   * @param vehicle : Vehicle to be parked
   * @return : True if vehicle is already parked, else returns false
   */
  def isVehicleParked(vehicle: Vehicle): Boolean = {
    for(lot <- 0 until parkingLot){
      for(capacity <- 0 until totalCapacity){
        if(vehicle.equals(parkingLotArray(lot)(capacity))){
          return true
        }
      }
    }
    false
  }

  /**
   *
   * @param vehicle : Vehicle to unpark
   * @return : return true if vehicle unparked, else false
   */
  def unPark(vehicle : Vehicle) : Boolean = {
    try{
      if(vehicle == null) return false
      if(parkingLot == 0){
        for(capacity <- 0 to totalCapacity-1)
        {
          if(parkingLotArray(capacity) != null) {
            parkingLotArray(0)(capacity) = null
            observers.foreach(observer => observer.capacityIsAvailable())
            return true
          }
        }
        return false
      }
      else{
        for(lot <- 0 until parkingLot){
          for(capacity <- 0 until totalCapacity){
            if(vehicle.equals(parkingLotArray(lot)(capacity))){
              parkingLotArray(lot)(capacity) = null
              observers.foreach(observer => observer.capacityIsAvailable())
              return true
            }
          }
        }
      }
      false
    }
    catch {
      case _: ParkingLotException => {
        println("Parking Lot Full")
        false
      }
      case ex: Exception => {
        println(ex.getMessage())
        false
      }
    }
  }

  /**
   *
   * @param colour : colour of vehicle
   * @param brand : brand of vehicle
   * @return : lot where vehicle is present
   */
  def getVehicleLocation(colour: String,brand:String*): Int = {
    try {
      for(lot <- 0 until parkingLot){
        for(capacity <- 0 until totalCapacity){
          if(colour.equals(parkingLotArray(lot)(capacity).vehicleColour) && brand.equals(parkingLotArray(lot)(capacity).vehicleBrand) ){
            return lot
          }
        }
      }
      -1
    }
    catch {
      case _: NullPointerException => {
        println("1")
        1
      }
      case exception: Exception => {
        println(exception.getMessage)
        1
      }
    }
  }

  /**
   *
   * @param brand : Brand to search vehicle
   * @return : count of vehicles found
   */
  def getVehicleWithBrand(brand:String):Int = {
    try {
      var count = 0
      for(lot <- 0 until parkingLot){
        for(capacity <- 0 until totalCapacity){
          if(brand.equals(parkingLotArray(lot)(capacity).vehicleBrand) ){
            count += 1
            count
          }
        }
      }
      -1
    }
    catch {
      case _: NullPointerException => {
        println("1")
        1
      }
      case exception: Exception => {
        println(exception.getMessage)
        1
      }
    }
  }

  /**
   *
   * @param driverType : normal or handicap
   * @param lot : location where we need to search
   * @return : count of vehicle found
   */
  def getDriverTypeLocation(driverType: String,lot:Int) = {
    try {
      var count = 0
      if(driverType.equals("handicap")){
        for(lot <- 0 until parkingLot){
          for(_ <- 0 until totalCapacity) {
            if(lot == lot)
              count += 1
            count
          }
        }
      }
      1
    }
    catch {
      case _: NullPointerException => {
        println("1")
        1
      }
      case exception: Exception => {
        println(exception.getMessage)
        1
      }
    }
  }
  // Returns vehicle parked in last 30 mins
  def getVehicleParked30minsBefore() = {
    try {
      var count = 0
      for(lot <- 0 until parkingLot){
        for(capacity <- 0 until totalCapacity){
          var time = parkingLotArray(lot)(capacity).parkTime(0).toInt
          time -= 30
          if(getTimeOfPark() == time.toString){
            count += 1
          }
        }
      }
      1
    }
    catch {
      case _: NullPointerException => {
        println("1")
        1
      }
      case exception: Exception => {
        println(exception.getMessage)
        1
      }
    }
  }
}
