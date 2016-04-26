
enum NSProgressIndicatorThickness : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case preferredThickness
  case preferredSmallThickness
  case preferredLargeThickness
  case preferredAquaThickness
}
enum NSProgressIndicatorStyle : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case barStyle
  case spinningStyle
}
class NSProgressIndicator : NSView, NSAccessibilityProgressIndicator {
  var isIndeterminate: Bool
  var isBezeled: Bool
  var controlTint: NSControlTint
  var controlSize: NSControlSize
  var doubleValue: Double
  func increment(by delta: Double)
  var minValue: Double
  var maxValue: Double
  var usesThreadedAnimation: Bool
  func startAnimation(_ sender: AnyObject?)
  func stopAnimation(_ sender: AnyObject?)
  var style: NSProgressIndicatorStyle
  func sizeToFit()
  var isDisplayedWhenStopped: Bool
}
struct __progressIndicatorFlags {
  var isSpinning: UInt32
  var isVector: UInt32
  var isLocked: UInt32
  var controlTint: UInt32
  var controlSize: UInt32
  var style: UInt32
  var _delayedStartup: UInt32
  var hideWhenStopped: UInt32
  var revive: UInt32
  var _temporarilyBlockHeartBeating: UInt32
  var _isHidden: UInt32
  var _isHeartBeatInstalled: UInt32
  var _customRenderer: UInt32
  var _lastFrame: UInt32
  var _isDetaching: UInt32
  var RESERVED: UInt32
  init()
  init(isSpinning isSpinning: UInt32, isVector isVector: UInt32, isLocked isLocked: UInt32, controlTint controlTint: UInt32, controlSize controlSize: UInt32, style style: UInt32, _delayedStartup _delayedStartup: UInt32, hideWhenStopped hideWhenStopped: UInt32, revive revive: UInt32, _temporarilyBlockHeartBeating _temporarilyBlockHeartBeating: UInt32, _isHidden _isHidden: UInt32, _isHeartBeatInstalled _isHeartBeatInstalled: UInt32, _customRenderer _customRenderer: UInt32, _lastFrame _lastFrame: UInt32, _isDetaching _isDetaching: UInt32, RESERVED RESERVED: UInt32)
}
extension NSProgressIndicator {
}
