package com.bridgelabz

class ParkingLotSystem
{
  var vehicle : Object = ""
  /**
   *
   * @param vehicle : Vehicle to park
   * @return : Boolean value if parked then true, else false
   */
  def park(vehicle : Object): Boolean = {
    if(this.vehicle != null) return false
      this.vehicle = vehicle
      true
  }

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
