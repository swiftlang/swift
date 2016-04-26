
struct CMRotationRate {
  var x: Double
  var y: Double
  var z: Double
  init()
  init(x x: Double, y y: Double, z z: Double)
}
@available(watchOS 2.0, *)
class CMGyroData : CMLogItem {
  var rotationRate: CMRotationRate { get }
}
