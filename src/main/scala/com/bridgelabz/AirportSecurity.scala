package com.bridgelabz

class AirportSecurity extends ParkingLotObserver {
  var isFullCapacity = false
  // Sets full capacity to true is capacity of parking lot is full
  def capacityIsFull(): Unit = {
    isFullCapacity = true
  }
  // sets full capacity to false when car unparked
  override def capacityIsAvailable(): Unit = {
    isFullCapacity = false
  }
  // returns whether capacity of parking lot is full ot not
  def isCapacityFull(): Boolean = {
    isFullCapacity
  }
}