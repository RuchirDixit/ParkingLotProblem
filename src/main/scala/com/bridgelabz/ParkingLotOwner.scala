package com.bridgelabz

class ParkingLotOwner {
  var isFullCapacity = false
  // Sets full capacity to true is capacity of parking lot is full
  def capacityIsFull(): Unit = {
    isFullCapacity = true
  }
  // returns whether capacity of parking lot is full ot not
  def isCapacityFull(): Boolean = {
    isFullCapacity
  }
}
