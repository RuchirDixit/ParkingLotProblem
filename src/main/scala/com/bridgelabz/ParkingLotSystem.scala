package com.bridgelabz

class ParkingLotSystem
{

  var vehicle : Object = ""
  /**
   *
   * @param vehicle : Vehicle to park
   * @return : Boolean value if parked then true, else false
   */
    @throws(classOf[ParkingLotException])
  def park(vehicle : Object): Unit = {
    if(this.vehicle != null){
      throw new ParkingLotException("Parking Lot Full")
    }
    this.vehicle = vehicle
  }
  def isVehicleParked(vehicle: Object): Boolean = {
    if(this.vehicle.equals(vehicle)) return true
    false
  }

  /**
   *
   * @param vehicle : Vehicle to unpark
   * @return : return true if vehicle unparked, else false
   */
  def unPark(vehicle : Object) : Boolean = {
    if(vehicle == null) return false
    if(this.vehicle.equals(vehicle))
    {
      this.vehicle = null
      return true
    }
    false
  }

}
