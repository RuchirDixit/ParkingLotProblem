package com.bridgelabz

import java.text.SimpleDateFormat
import java.util.Calendar

import scala.collection.mutable.ListBuffer
// Parking Lot class to handle park and unPark
class ParkingLotSystem(parkingLotCapacity:Int)
{
  // ListBuffer to store vehicles in parking lot
  var vehicles = new ListBuffer[Object]
  var observers = new ListBuffer[ParkingLotObserver]
  var totalCapacity = parkingLotCapacity
  var timingOfParking = ""

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
    else if(this.vehicles.size >= slotNumber) false
    else true
  }
  /**
   *
   * @param vehicle : Vehicle to park
   * @return : Boolean value if parked then true, else false
   */
    @throws(classOf[ParkingLotException])
  def park(vehicle : Object): Unit =
    {
      try {
        getTimeOfPark()
        if (isVehicleParked(vehicle)) {
          throw new ParkingLotException("Vehicle already parked")
        }
        if (this.vehicles.size == totalCapacity) {
          observers.foreach(observer => observer.capacityIsFull())
          throw new ParkingLotException("Parking Lot Full")
        }
        this.vehicles += vehicle
        if (this.vehicles.size == totalCapacity) {
          observers.foreach(observer => observer.capacityIsFull())
        }
      }
        catch {
          case _: ParkingLotException => {
            println("Parking Lot Full")
          }
          case ex: Exception => {
            println(ex.getMessage())
          }
        }
  }
  def getTimeOfPark() : Unit = {
    val calender = Calendar.getInstance();
    val hour = new SimpleDateFormat("hh");
    val hours = hour.format(calender.getTime());
    val min = new SimpleDateFormat("mm");
    val minutes = min.format(calender.getTime());
    timingOfParking = hour + "" + minutes
  }
  /**
   *
   * @param vehicle : Vehicle to be parked
   * @return : True if vehicle is already parked, else returns false
   */
  def isVehicleParked(vehicle: Object): Boolean = {
    if(this.vehicles.contains(vehicle)) return true
    false
  }

  /**
   *
   * @param vehicle : Vehicle to unpark
   * @return : return true if vehicle unparked, else false
   */
  def unPark(vehicle : Object) : Boolean = {
    try{
      if(vehicle == null) return false
      if(this.vehicles.contains(vehicle))
      {
        this.vehicles -= vehicle
        observers.foreach(observer => observer.capacityIsAvailable())
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
}
