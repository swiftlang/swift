
struct GCAcceleration {
  var x: Double
  var y: Double
  var z: Double
  init()
  init(x x: Double, y y: Double, z z: Double)
}
struct GCRotationRate {
  var x: Double
  var y: Double
  var z: Double
  init()
  init(x x: Double, y y: Double, z z: Double)
}
struct GCEulerAngles {
  var pitch: Double
  var yaw: Double
  var roll: Double
  init()
  init(pitch pitch: Double, yaw yaw: Double, roll roll: Double)
}
struct GCQuaternion {
  var x: Double
  var y: Double
  var z: Double
  var w: Double
  init()
  init(x x: Double, y y: Double, z z: Double, w w: Double)
}
@available(iOS 8.0, *)
class GCMotion : NSObject {
  weak var controller: @sil_weak GCController? { get }
  var valueChangedHandler: GCMotionValueChangedHandler?
  var gravity: GCAcceleration { get }
  var userAcceleration: GCAcceleration { get }
  var attitude: GCQuaternion { get }
  var rotationRate: GCRotationRate { get }
}
typealias GCMotionValueChangedHandler = (GCMotion) -> Void
