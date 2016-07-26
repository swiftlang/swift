
@available(OSX 10.7, *)
struct NSTouchPhase : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var began: NSTouchPhase { get }
  static var moved: NSTouchPhase { get }
  static var stationary: NSTouchPhase { get }
  static var ended: NSTouchPhase { get }
  static var cancelled: NSTouchPhase { get }
  static var touching: NSTouchPhase { get }
  static var any: NSTouchPhase { get }
}
@available(OSX 10.6, *)
class NSTouch : NSObject, NSCopying {
  var identity: protocol<NSCopying, NSObjectProtocol> { get }
  var phase: NSTouchPhase { get }
  var normalizedPosition: NSPoint { get }
  var isResting: Bool { get }
  var device: AnyObject? { get }
  var deviceSize: NSSize { get }
}
