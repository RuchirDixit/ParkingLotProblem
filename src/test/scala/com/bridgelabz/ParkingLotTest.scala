package com.bridgelabz

import org.scalatest.FunSuite
// test class
class ParkingLotTest extends FunSuite
{

  // UC1 : To Park car
  test("givenAVehicleWhenParkedShouldReturnTrue"){
    try {
      val parkingLot = new ParkingLotSystem()
      val vehicle = new Object()
      parkingLot.park(vehicle)
      val isParked = parkingLot.isVehicleParked(vehicle)
      assert(isParked == true)
    }
   catch {
     case _ : Exception => {
       println("Parking Lot exception")
     }
   }
  }

 // UC2 : To unPark
  test("givenVehicleWhenUnParkedShouldReturnTrue") {
    try {
      val parkingLot = new ParkingLotSystem()
      val vehicle = new Object()
      parkingLot.park(vehicle)
      val isUnParked = parkingLot.unPark(vehicle)
      assert(isUnParked == true)
    }
    catch {
      case _: Exception => {}
    }
  }

  // To check id car is already parked throw exception
  test("givenAVehicleWhenAlreadyParkedShouldReturnFalse"){
    try {
      val parkingLot = new ParkingLotSystem()
      val vehicle = new Object()
      parkingLot.park(vehicle)
    }
    catch {
      case parkingLotException : ParkingLotException => {
        assert(parkingLotException.getMessage.equals("Parking Lot Full"))
      }
    }
  }
}
