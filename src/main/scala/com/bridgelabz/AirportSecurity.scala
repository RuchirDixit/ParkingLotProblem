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

import com.typesafe.scalalogging.LazyLogging

class AirportSecurity extends ParkingLotObserver with LazyLogging{
  var isFullCapacity = false
  // Sets full capacity to true is capacity of parking lot is full
  def capacityIsFull(): Unit = {
    logger.info("Parking lot capacity full")
    isFullCapacity = true
  }
  // sets full capacity to false when car unparked
  // $COVERAGE-OFF$
  override def capacityIsAvailable(): Boolean = {
    logger.info("Parking lot available")
    isFullCapacity = false
    false
  }
  // returns whether capacity of parking lot is full ot not
  def isCapacityFull(): Boolean = {
    isFullCapacity
  }
}
