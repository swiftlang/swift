
enum NSSegmentSwitchTracking : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case selectOne
  case selectAny
  case momentary
  @available(OSX 10.10.3, *)
  case momentaryAccelerator
}
@available(OSX 10.5, *)
enum NSSegmentStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case automatic
  case rounded
  case roundRect
  case texturedSquare
  case smallSquare
  @available(OSX 10.10, *)
  case separated
  @available(OSX 10.5, *)
  case texturedRounded
  @available(OSX 10.5, *)
  case capsule
}
class NSSegmentedControl : NSControl {
  var segmentCount: Int
  var selectedSegment: Int
  @discardableResult
  func selectSegment(withTag tag: Int) -> Bool
  func setWidth(_ width: CGFloat, forSegment segment: Int)
  @discardableResult
  func width(forSegment segment: Int) -> CGFloat
  func setImage(_ image: NSImage?, forSegment segment: Int)
  @discardableResult
  func image(forSegment segment: Int) -> NSImage?
  @available(OSX 10.5, *)
  func setImageScaling(_ scaling: NSImageScaling, forSegment segment: Int)
  @available(OSX 10.5, *)
  @discardableResult
  func imageScaling(forSegment segment: Int) -> NSImageScaling
  func setLabel(_ label: String, forSegment segment: Int)
  @discardableResult
  func label(forSegment segment: Int) -> String?
  func setMenu(_ menu: NSMenu?, forSegment segment: Int)
  @discardableResult
  func menu(forSegment segment: Int) -> NSMenu?
  func setSelected(_ selected: Bool, forSegment segment: Int)
  @discardableResult
  func isSelected(forSegment segment: Int) -> Bool
  func setEnabled(_ enabled: Bool, forSegment segment: Int)
  @discardableResult
  func isEnabled(forSegment segment: Int) -> Bool
  @available(OSX 10.5, *)
  var segmentStyle: NSSegmentStyle
  @available(OSX 10.10.3, *)
  var isSpringLoaded: Bool
  @available(OSX 10.10.3, *)
  var trackingMode: NSSegmentSwitchTracking
  @available(OSX 10.10.3, *)
  var doubleValueForSelectedSegment: Double { get }
}
