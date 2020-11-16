package com.bridgelabz

import org.scalatest.FunSuite
// test class
class ParkingLotTest extends FunSuite
{

  // UC1 : To Park car
  test("givenAVehicleWhenParkedShouldReturnTrue"){
    val parkingLot = new ParkingLotSystem()
    val isParked = parkingLot.park(new Object)
    assert(isParked == true)
  }
}
