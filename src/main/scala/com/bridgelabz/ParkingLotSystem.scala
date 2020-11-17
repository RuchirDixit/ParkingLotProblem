package com.bridgelabz

import scala.collection.mutable.ListBuffer
// Parking Lot class to handle park and unPark
class ParkingLotSystem(parkingLotCapacity:Int)
{
  // ListBuffer to store vehicles in parking lot
  var vehicles = new ListBuffer[Object]
  var owner : ParkingLotOwner = null
  var totalCapacity = parkingLotCapacity

  /**
   *
   * @param capacity : Set total capacity of parking lot
   */
  def setCapacity(capacity: Int): Unit ={
    this.totalCapacity = capacity
  }

  /**
   *
   * @param owner : To register as a parking lot owner
   */
  def registerOwner(owner: ParkingLotOwner) = {
    this.owner = owner
  }
  /**
   *
   * @param vehicle : Vehicle to park
   * @return : Boolean value if parked then true, else false
   */
    @throws(classOf[ParkingLotException])
  def park(vehicle : Object): Unit = {
    if(this.vehicles.size == totalCapacity) {
      owner.capacityIsFull()
      throw new ParkingLotException("Parking Lot Full")
      }
      if(isVehicleParked(vehicle)) {
        throw new ParkingLotException("Vehicle already parked")
      }
      this.vehicles += vehicle
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
    if(vehicle == null) return false
    if(this.vehicles.contains(vehicle))
    {
      this.vehicles -= vehicle
      return true
    }
    false
  }
}
