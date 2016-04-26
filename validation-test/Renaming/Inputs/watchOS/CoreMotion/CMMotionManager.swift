
typealias CMAccelerometerHandler = (CMAccelerometerData?, NSError?) -> Void
typealias CMGyroHandler = (CMGyroData?, NSError?) -> Void
typealias CMDeviceMotionHandler = (CMDeviceMotion?, NSError?) -> Void
@available(watchOS 2.0, *)
typealias CMMagnetometerHandler = (CMMagnetometerData?, NSError?) -> Void
@available(watchOS 2.0, *)
class CMMotionManager : NSObject {
  var accelerometerUpdateInterval: NSTimeInterval
  var isAccelerometerAvailable: Bool { get }
  var isAccelerometerActive: Bool { get }
  var accelerometerData: CMAccelerometerData? { get }
  func startAccelerometerUpdates()
  func startAccelerometerUpdates(to queue: NSOperationQueue, withHandler handler: CMAccelerometerHandler)
  func stopAccelerometerUpdates()
  var gyroUpdateInterval: NSTimeInterval
  var isGyroAvailable: Bool { get }
  var isGyroActive: Bool { get }
  var gyroData: CMGyroData? { get }
  func startGyroUpdates()
  func startGyroUpdates(to queue: NSOperationQueue, withHandler handler: CMGyroHandler)
  func stopGyroUpdates()
  @available(watchOS 2.0, *)
  var magnetometerUpdateInterval: NSTimeInterval
  @available(watchOS 2.0, *)
  var isMagnetometerAvailable: Bool { get }
  @available(watchOS 2.0, *)
  var isMagnetometerActive: Bool { get }
  @available(watchOS 2.0, *)
  var magnetometerData: CMMagnetometerData? { get }
  @available(watchOS 2.0, *)
  func startMagnetometerUpdates()
  @available(watchOS 2.0, *)
  func startMagnetometerUpdates(to queue: NSOperationQueue, withHandler handler: CMMagnetometerHandler)
  @available(watchOS 2.0, *)
  func stopMagnetometerUpdates()
  var deviceMotionUpdateInterval: NSTimeInterval
  @available(watchOS 2.0, *)
  @discardableResult
  class func availableAttitudeReferenceFrames() -> CMAttitudeReferenceFrame
  @available(watchOS 2.0, *)
  var attitudeReferenceFrame: CMAttitudeReferenceFrame { get }
  var isDeviceMotionAvailable: Bool { get }
  var isDeviceMotionActive: Bool { get }
  var deviceMotion: CMDeviceMotion? { get }
  func startDeviceMotionUpdates()
  func startDeviceMotionUpdates(to queue: NSOperationQueue, withHandler handler: CMDeviceMotionHandler)
  @available(watchOS 2.0, *)
  func startDeviceMotionUpdates(using referenceFrame: CMAttitudeReferenceFrame)
  @available(watchOS 2.0, *)
  func startDeviceMotionUpdates(using referenceFrame: CMAttitudeReferenceFrame, to queue: NSOperationQueue, withHandler handler: CMDeviceMotionHandler)
  func stopDeviceMotionUpdates()
  @available(watchOS 2.0, *)
  var showsDeviceMovementDisplay: Bool
}
