
struct CMMagneticField {
  var x: Double
  var y: Double
  var z: Double
  init()
  init(x x: Double, y y: Double, z z: Double)
}
@available(iOS 5.0, *)
class CMMagnetometerData : CMLogItem {
  var magneticField: CMMagneticField { get }
}
