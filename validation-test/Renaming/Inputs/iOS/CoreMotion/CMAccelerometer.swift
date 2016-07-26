
struct CMAcceleration {
  var x: Double
  var y: Double
  var z: Double
  init()
  init(x x: Double, y y: Double, z z: Double)
}
@available(iOS 4.0, *)
class CMAccelerometerData : CMLogItem {
  var acceleration: CMAcceleration { get }
}
