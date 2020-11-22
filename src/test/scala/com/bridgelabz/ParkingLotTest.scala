package com.bridgelabz

import org.scalatest.FunSuite
// test class
class ParkingLotTest extends FunSuite
{

  // UC1 : To Park car
  test("givenAVehicleWhenParkedShouldReturnTrue"){
    try {
      val parkingLot = new ParkingLotSystem(1)
      val vehicle = new Vehicle("","","")
      parkingLot.park(vehicle,"normal")
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
      val vehicle = new Vehicle("","","")
      parkingLot.park(vehicle,"normal")
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
      val vehicle = new Vehicle("","","")
      parkingLot.park(vehicle,"normal")
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
      val vehicle = new Vehicle("","","")
      parkingLot.park(vehicle,"normal")
      parkingLot.park(new Vehicle("","",""),"normal")
    }
    catch {
      case ex : ParkingLotException => {
        println(ex.getMessage())
      }
    }
    val capacity = owner.isCapacityFull()
    assert(capacity == true)
  }

  test("givenCapacityIs2ShouldBeAbleToParkTwoVehicles"){
    val parkingLot = new ParkingLotSystem(1,1)
    parkingLot.setCapacity(2)
    try
      {
        val vehicle = new Vehicle("","","")
        val vehicle2 = new Vehicle("","","")
      parkingLot.park(vehicle,"normal")
      parkingLot.park(vehicle2,"normal")
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
    val parkingLot = new ParkingLotSystem(1,1)
    parkingLot.registerParkingLotObserver(airport)
    try{
      val vehicle = new Vehicle("","","")
      parkingLot.park(vehicle,"normal")
      parkingLot.park(new Vehicle("","",""),"normal")
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
    val parkingLot = new ParkingLotSystem(1,1)
    parkingLot.registerParkingLotObserver(owner)
    parkingLot.setCapacity(2)
    val vehicle = new Vehicle("","","")
    val vehicle2 = new Vehicle("","","")
    parkingLot.park(vehicle,"normal")
    parkingLot.park(vehicle2,"normal")
    parkingLot.unPark(vehicle)
    val capacity = owner.isCapacityFull()
    assert(capacity == false)
  }

  // UC6 : If Parking Lot empty should park wherever possible
  test("givenAnEmptyParkingLotWhenAskedWhereToParkShouldReturnTrue"){
    val parkingLot = new ParkingLotSystem(2)
    val status =  parkingLot.parkAtSlot(1)
    assert(status == true)
  }
  // If asked to park at place where already vehicle parked
  test("givenParkingLotWhenAskedWhereToParkAndIfAlreadyParkingFullOrParkedShouldReturnFalse"){
    val parkingLot = new ParkingLotSystem(2,1)
    val vehicle = new Vehicle("","","")
    val vehicle2 = new Vehicle("","","")
    parkingLot.park(vehicle,"normal")
    parkingLot.park(vehicle2,"normal")
    val status =  parkingLot.parkAtSlot(1)
    assert(status == false)
  }
  // UC7 : Find vehicle
  test("givenVehicleIfFoundShouldReturnTrue"){
    val parkingLot = new ParkingLotSystem(2,1)
    val vehicle = new Vehicle("","","")
    parkingLot.park(vehicle,"normal")
    val vehicleFound = parkingLot.isVehicleParked(vehicle)
    assert(vehicleFound == true)
  }
  // If vehicle not found
  test("givenVehicleIfNotFoundShouldReturnFalse"){
    val parkingLot = new ParkingLotSystem(2)
    val vehicle = new Vehicle("","","")
    parkingLot.park(vehicle,"normal")
    val vehicleFound = parkingLot.isVehicleParked(new Vehicle("","",""))
    assert(vehicleFound == false)
  }
  // UC8
  test("givenVehicleWhenParkedShouldReturnTime"){
    val parkingLot = new ParkingLotSystem(2)
    val vehicle = new Vehicle("","","")
    parkingLot.park(vehicle,"normal")
    assert(parkingLot.timingOfParking != "")
  }
  // UC9
  test("givenVehicleWhenParkedShouldBeEvenlyDistributedReturnTrue"){
    val parkingLot = new ParkingLotSystem(2,2)
    val vehicle = new Vehicle("","","")
    assert(parkingLot.park(vehicle,"normal") == true)
  }
  // UC10
  test("givenVehicleWithHandicapDriverFindNearestSpotAndReturnTrue"){
    val parkingLot = new ParkingLotSystem(2,2)
    val vehicle = new Vehicle("","","")
    assert(parkingLot.park(vehicle,"Handicap") == true)
  }
  // UC11
  test("givenLargeVehicleFindSpotWithMoreSpaceAndReturnTrue"){
    val parkingLot = new ParkingLotSystem(2,2)
    val vehicle = new Vehicle("","","")
    val status = parkingLot.park(vehicle,"normal","Large")
    assert(status == true)
  }
  // UC12
  test("givenWhiteVehicleShouldReturnLotWhereFound"){
    val parkingLot = new ParkingLotSystem(2,1)
    val vehicle = new Vehicle("MH12 AA0000","White","Toyota")
    parkingLot.park(vehicle,"normal")
    val vehicle2 = new Vehicle("MH12 ZZ9999","Black","BMW")
    parkingLot.park(vehicle2,"normal")
    val lot = parkingLot.getVehicleLocation("White")
    assert(lot == 1)
  }
  // UC13
  test("givenWhiteVehicleWithBrandToyotaShouldReturnLotWhereFound"){
    val parkingLot = new ParkingLotSystem(2,1)
    val vehicle = new Vehicle("MH12 AA0000","White","Toyota")
    parkingLot.park(vehicle,"normal")
    val vehicle2 = new Vehicle("MH12 ZZ9999","Black","BMW")
    parkingLot.park(vehicle2,"normal")
    val lot = parkingLot.getVehicleLocation("White","Toyota")
    assert(lot == 1)
  }
  // UC 14
  test("givenVehiclesWithBMWShouldReturnTrue"){
    val parkingLot = new ParkingLotSystem(2,1)
    val vehicle = new Vehicle("MH12 AA0000","White","Toyota")
    parkingLot.park(vehicle,"normal")
    val vehicle2 = new Vehicle("MH12 ZZ9999","Black","BMW")
    parkingLot.park(vehicle2,"normal")
    val lot = parkingLot.getVehicleWithBrand("BMW")
    assert(lot == 1)
  }

  // UC 15
  test("givenVehiclesParkedBefore30MinsShouldReturnCount"){
    val parkingLot = new ParkingLotSystem(2,1)
    val vehicle = new Vehicle("MH12 AA0000","White","Toyota",parkingLot.getTimeOfPark())
    parkingLot.park(vehicle,"normal")
    val vehicle2 = new Vehicle("MH12 ZZ9999","Black","BMW",parkingLot.getTimeOfPark())
    parkingLot.park(vehicle2,"normal")
    val lot = parkingLot.getVehicleParked30minsBefore()
    assert(lot == 1)
  }
}
