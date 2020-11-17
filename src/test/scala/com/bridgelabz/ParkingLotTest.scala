package com.bridgelabz

import org.scalatest.FunSuite
// test class
class ParkingLotTest extends FunSuite
{

  // UC1 : To Park car
  test("givenAVehicleWhenParkedShouldReturnTrue"){
    try {
      val parkingLot = new ParkingLotSystem(1)
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
      val parkingLot = new ParkingLotSystem(1)
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
      val parkingLot = new ParkingLotSystem(1)
      val vehicle = new Object()
      parkingLot.park(vehicle)
    }
    catch {
      case parkingLotException : ParkingLotException => {
        assert(parkingLotException.getMessage.equals("Parking Lot Full"))
      }
    }
  }

  // UC3 : Inform owner on parking lot full
  test("givenWhenParkingLotIsFullShouldInformOwner"){
    val owner = new ParkingLotOwner()
    val parkingLot = new ParkingLotSystem(1)
    parkingLot.registerParkingLotObserver(owner)
    try{
      val vehicle = new Object()
      parkingLot.park(vehicle)
      parkingLot.park(new Object())
    }
    catch {
      case ex : ParkingLotException => {
        println(ex.getMessage())
      }
    }
    val capacity = owner.isCapacityFull()
    assert(capacity == true)
  }

  test("givenCapacityIsT2ShouldBeAbleToParkTwoVehicles"){
    val parkingLot = new ParkingLotSystem(1)
    parkingLot.setCapacity(2)
    try
      {
      val vehicle = new Object()
        val vehicle2 = new Object()
      parkingLot.park(vehicle)
      parkingLot.park(vehicle2)
        val isParked1 = parkingLot.isVehicleParked(vehicle)
        val isParked2 = parkingLot.isVehicleParked(vehicle2)
        assert(isParked1 == true && isParked2 == true)
    }
    catch {
      case ex : ParkingLotException => {
        println(ex.getMessage())
      }
    }
  }
  // UC4: Inform Airport security
  test("givenWhenParkingLotIsFullShouldInformAirportSecurity"){
    val airport = new AirportSecurity()
    val parkingLot = new ParkingLotSystem(1)
    parkingLot.registerParkingLotObserver(airport)
    try{
      val vehicle = new Object()
      parkingLot.park(vehicle)
      parkingLot.park(new Object())
    }
    catch {
      case ex : ParkingLotException => {
        println(ex.getMessage())
      }
    }
    val isCapacityFull = airport.isCapacityFull()
    assert(isCapacityFull == true)
  }

  // UC5
  test("givenWhenParkingLotSpaceAvailableAfterFullShouldReturnTrue"){
    val owner = new ParkingLotOwner()
    val parkingLot = new ParkingLotSystem(1)
    parkingLot.registerParkingLotObserver(owner)
    parkingLot.setCapacity(2)
    val vehicle = new Object()
    val vehicle2 = new Object()
    parkingLot.park(vehicle)
    parkingLot.park(vehicle2)
    parkingLot.unPark(vehicle)
    val capacity = owner.isCapacityFull()
    assert(capacity == false)
  }
}
