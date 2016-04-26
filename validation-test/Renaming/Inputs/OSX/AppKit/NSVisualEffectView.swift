
@available(OSX 10.10, *)
enum NSVisualEffectMaterial : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case appearanceBased
  case titlebar
  @available(OSX 10.11, *)
  case menu
  @available(OSX 10.11, *)
  case popover
  @available(OSX 10.11, *)
  case sidebar
  case light
  case dark
  @available(OSX 10.11, *)
  case mediumLight
  @available(OSX 10.11, *)
  case ultraDark
}
@available(OSX 10.10, *)
enum NSVisualEffectBlendingMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case behindWindow
  case withinWindow
}
@available(OSX 10.10, *)
enum NSVisualEffectState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case followsWindowActiveState
  case active
  case inactive
}
@available(OSX 10.10, *)
class NSVisualEffectView : NSView {
  var material: NSVisualEffectMaterial
  var interiorBackgroundStyle: NSBackgroundStyle { get }
  var blendingMode: NSVisualEffectBlendingMode
  var state: NSVisualEffectState
  var maskImage: NSImage?
}
