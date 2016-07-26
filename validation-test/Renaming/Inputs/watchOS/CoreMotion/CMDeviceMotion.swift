
struct CMMagneticFieldCalibrationAccuracy : RawRepresentable, Equatable {
  init(_ rawValue: Int32)
  init(rawValue rawValue: Int32)
  var rawValue: Int32
}
var CMMagneticFieldCalibrationAccuracyUncalibrated: CMMagneticFieldCalibrationAccuracy { get }
var CMMagneticFieldCalibrationAccuracyLow: CMMagneticFieldCalibrationAccuracy { get }
var CMMagneticFieldCalibrationAccuracyMedium: CMMagneticFieldCalibrationAccuracy { get }
var CMMagneticFieldCalibrationAccuracyHigh: CMMagneticFieldCalibrationAccuracy { get }
struct CMCalibratedMagneticField {
  var field: CMMagneticField
  var accuracy: CMMagneticFieldCalibrationAccuracy
  init()
  init(field field: CMMagneticField, accuracy accuracy: CMMagneticFieldCalibrationAccuracy)
}
@available(watchOS 2.0, *)
class CMDeviceMotion : CMLogItem {
  var attitude: CMAttitude { get }
  var rotationRate: CMRotationRate { get }
  var gravity: CMAcceleration { get }
  var userAcceleration: CMAcceleration { get }
  @available(watchOS 2.0, *)
  var magneticField: CMCalibratedMagneticField { get }
}
