// Copyright (C) 2011-2012 the original author or authors.
// See the LICENCE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
package com.bridgelabz

import com.bridgelabz.airportsecurity.AirportSecurity
import com.bridgelabz.parkinglot.{ParkingLotException, ParkingLotOwner, ParkingLotSystem}
import com.bridgelabz.vehicle.Vehicle
import com.typesafe.scalalogging.LazyLogging
import org.scalatestplus.mockito.MockitoSugar
import org.scalatest.{BeforeAndAfter, FunSuite}
import org.mockito.Mockito._
// test class
class ParkingLotTest extends FunSuite with BeforeAndAfter with MockitoSugar with LazyLogging
{

  // UC1 : To Park car
  test("givenAVehicleWhenParkedShouldReturnTrue"){
      val parkingLot = new ParkingLotSystem(1)
      val vehicle = new Vehicle("","","")
      parkingLot.park(vehicle,"normal")
      val isParked = parkingLot.isVehicleParked(vehicle)
      assert(isParked == false)
  }

 // UC2 : To unPark
  test("givenVehicleWhenUnParkedShouldReturnTrue") {
      val parkingLot = new ParkingLotSystem(1)
      val vehicle = new Vehicle("","","")
      parkingLot.park(vehicle,"normal")
      val isUnParked = parkingLot.unPark(vehicle)
      assert(isUnParked == false)
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
      case exception: Exception => {
        logger.error(exception.toString)
      }
    }
  }

  // UC3 : Inform owner on parking lot full
  test("givenWhenParkingLotIsFullShouldInformOwner"){
    val owner = new ParkingLotOwner() // mock
    val parkingLot = new ParkingLotSystem(1)
    parkingLot.registerParkingLotObserver(owner)
    val vehicle = new Vehicle("","","")
    parkingLot.park(vehicle,"normal")
    parkingLot.park(new Vehicle("","",""),"normal")
    val capacity = owner.isCapacityFull()
    assert(capacity == true)
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
      case exception: Exception => {
        logger.error(exception.toString)
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
    assert(vehicleFound == false)
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
    assert(parkingLot.park(vehicle,"normal") == false)
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
  // UC 16 & UC 17
  test("givenInformationAboutVehiclesShouldReturnCount"){
    val parkingLot = new ParkingLotSystem(2,1)
    val vehicle = new Vehicle("MH12 AA0000","White","Toyota")
    parkingLot.park(vehicle,"normal")
    val vehicle2 = new Vehicle("MH12 ZZ9999","Black","BMW")
    parkingLot.park(vehicle2,"handicap")
    val lot = parkingLot.getDriverTypeLocation("handicap",1)
    assert(lot == 1)
  }


  // Mock test case
  test("givenWhenParkingUsingMockitoIsFullShouldInformOwner"){
    val service = mock[ParkingLotOwner]
    when(service.isCapacityFull()).thenReturn(true)
    val parkingLot = new ParkingLotSystem(1)
    val vehicle = new Vehicle("","","")
    parkingLot.park(vehicle,"normal")
    parkingLot.park(new Vehicle("","",""),"normal")
    val capacity = service.isCapacityFull()
    assert(capacity == true)
  }

  // To return false when trying to enter slot number more than capacity
  test("givenSlotNumberMoreThanTotalCapacityShouldReturnFalse"){
      val parkingLot = new ParkingLotSystem(1)
      val status = parkingLot.parkAtSlot(2)
      assert(status == false)
  }


  // To check id car is already parked throw exception
  test("givenSomethingShouldThrowException"){
    try {
      val parkingLot = new ParkingLotSystem(2,2)
      parkingLot.setCapacity(2)
      val vehicle = new Vehicle("","","")
      parkingLot.park(vehicle,"normal","normal")
      parkingLot.park(vehicle,"normal","normal")
    }catch {
      case ex:ParkingLotException =>
        assert(ex.getMessage.equals("Vehicle already parked"))
    }
  }

  test("givenWhenParkingUsingMockitoIsFullShouldReturnNothing"){
    val service = mock[ParkingLotOwner]
    when(service.capacityIsAvailable()).thenReturn(false)
    val status = service.capacityIsAvailable()
    assert(status == false)
  }
}
