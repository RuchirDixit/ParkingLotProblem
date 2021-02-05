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

import java.text.SimpleDateFormat
import java.util.Calendar

import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer
// Parking Lot class to handle park and unPark
class ParkingLotSystem(parkingLotCapacity:Int,parkingLot:Int = 0) extends LazyLogging
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
   * method to set total capacity of parking lot
   * @param capacity : Set total capacity of parking lot
   */
  def setCapacity(capacity: Int): Unit ={
    this.totalCapacity = capacity
    logger.info("capacity:" + this.totalCapacity)
  }

  /**
   * method to add observers to the list
   * @param observer: To register as an observer
   */
  def registerParkingLotObserver(observer: ParkingLotObserver) : Unit = {
    this.observers += observer
    logger.info("observers:" + this.observers)
  }

  /**
   * To check if slot is available and park at that slot
   * @param slotNumber : slot number at which vehicle should be parked
   * @return : If slot available true else false
   */
  def parkAtSlot(slotNumber: Int): Boolean = {
    if(slotNumber > totalCapacity) {
      false
    }
    else if(this.parkingLotArray.size >= slotNumber) {
      false
    }
    else {
      true
    }
  }

  /**
   * To park the vehicle at vacant spot according to type of driver and vehicle
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
          logger.error("vehicle already parked exception")
          throw new ParkingLotException("Vehicle already parked")
        }
        if(parkingLotArray.size == parkingLot*totalCapacity)
        {
          observers.foreach(observer => observer.capacityIsFull())
          logger.error("parking lot full exception")
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
            logger.info("Inside driver type normal")
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
            logger.info("Parking Lot Full")
            false
          }
          case ex: Exception => {
            logger.error(ex.getMessage())
            false
          }
        }
  }

  /**
   * method to park at first spot available for even distribution of vehicles inside parking lot
   * @param vehicle : Vehicle to park
   * @param vehicleType : Large or normal vehicle
   * @return
   */
  def normalDriverType(vehicle : Vehicle,vehicleType:String) : Boolean = {
    for(lot <- 0 to parkingLot-1){
      for(capacity <- 0 to totalCapacity-1){
        if(vehicleType.equals("Large"))
        {
          logger.info("Inside vehicle type large")
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
            if(lot + 1 <= parkingLot-1){
              if(parkingLotArray(lot + 1)(capacity) == null){
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

  /**
   * method is used to return the timing at which vehicle was parked
   * @return : String with timing of park
   */
  def getTimeOfPark() : String = {
    val calender = Calendar.getInstance();
    val hour = new SimpleDateFormat("hh");
    val hours = hour.format(calender.getTime());
    val min = new SimpleDateFormat("mm");
    val minutes = min.format(calender.getTime());
    timingOfParking = hour + "" + minutes
    logger.info("timing of parking" + timingOfParking)
    timingOfParking
  }

  /**
   * method to check whether vehicle is already parked at that slot or not
   * @param vehicle : Vehicle to be parked
   * @return : True if vehicle is already parked, else returns false
   */
  def isVehicleParked(vehicle: Vehicle): Boolean = {
    for(lot <- 0 until parkingLot){
      for(capacity <- 0 until totalCapacity){
        if(vehicle.equals(parkingLotArray(lot)(capacity))){
          logger.info("spot not vacant")
          return true
        }
      }
    }
    false
  }

  /**
   * method to unpark the vehicle from the slot
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
        logger.info("Parking Lot Full")
        false
      }
      case ex: Exception => {
        logger.error(ex.getMessage())
        false
      }
    }
  }

  /**
   * method to return lot at which vehicle is parked
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
        logger.info("1")
        1
      }
      case exception: Exception => {
        logger.error(exception.getMessage)
        1
      }
    }
  }

  /**
   * searches for vehicle with mentioned brand and increments counter based on search
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
        logger.info("1")
        1
      }
      case exception: Exception => {
        logger.error(exception.getMessage)
        1
      }
    }
  }

  /**
   * searches for vehicle with mentioned driver type and increments counter based on search
   * @param driverType : normal or handicap
   * @param lot : location where we need to search
   * @return : count of vehicle found
   */
  def getDriverTypeLocation(driverType: String,lot:Int) : 1 = {
    try {
      var count = 0
      if(driverType.equals("handicap")){
        for(lot <- 0 until parkingLot){
          for(_ <- 0 until totalCapacity) {
            if(lot == lot) {
              count += 1
            }
            count
          }
        }
      }
      1
    }
    catch {
      case _: NullPointerException => {
        logger.info("1")
        1
      }
      case exception: Exception => {
        logger.error(exception.getMessage)
        1
      }
    }
  }

  /**
   * method to get count of vehicles that were parked 30 mins before current time
   * @return : count of vehicles parked 30 mins before
   */
  def getVehicleParked30minsBefore() : 1 = {
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
        logger.info("1")
        1
      }
      case exception: Exception => {
        logger.error(exception.getMessage)
        1
      }
    }
  }
}
