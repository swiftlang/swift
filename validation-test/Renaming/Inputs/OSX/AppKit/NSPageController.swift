
@available(OSX 10.8, *)
enum NSPageControllerTransitionStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case stackHistory
  case stackBook
  case horizontalStrip
}
@available(OSX 10.8, *)
class NSPageController : NSViewController, NSAnimatablePropertyContainer, NSCoding {
  @IBOutlet unowned(unsafe) var delegate: @sil_unmanaged NSPageControllerDelegate?
  var selectedViewController: NSViewController? { get }
  var transitionStyle: NSPageControllerTransitionStyle
  var arrangedObjects: [AnyObject]
  var selectedIndex: Int
  func navigateForward(to object: AnyObject)
  func completeTransition()
  @IBAction func navigateBack(_ sender: AnyObject?)
  @IBAction func navigateForward(_ sender: AnyObject?)
  @IBAction func takeSelectedIndexFrom(_ sender: AnyObject?)
}
struct __pcDelegateFlags {
  var delegateRespondsToIdentifierForRepresentedObject: UInt32
  var delegateRespondsToViewControllerForIdentifier: UInt32
  var delegateRespondsToFrameForRepresentedObject: UInt32
  var delegateRespondsToPrepareView: UInt32
  var delegateRespondsToDidTransition: UInt32
  var delegateRespondsToWillLiveTransition: UInt32
  var delegateRespondsToDidLiveTransition: UInt32
  var delegateRespondsToReserved1: UInt32
  var reserved: UInt32
  init()
  init(delegateRespondsToIdentifierForRepresentedObject delegateRespondsToIdentifierForRepresentedObject: UInt32, delegateRespondsToViewControllerForIdentifier delegateRespondsToViewControllerForIdentifier: UInt32, delegateRespondsToFrameForRepresentedObject delegateRespondsToFrameForRepresentedObject: UInt32, delegateRespondsToPrepareView delegateRespondsToPrepareView: UInt32, delegateRespondsToDidTransition delegateRespondsToDidTransition: UInt32, delegateRespondsToWillLiveTransition delegateRespondsToWillLiveTransition: UInt32, delegateRespondsToDidLiveTransition delegateRespondsToDidLiveTransition: UInt32, delegateRespondsToReserved1 delegateRespondsToReserved1: UInt32, reserved reserved: UInt32)
}
struct __pcFlags {
  var templateCacheIsInvalid: UInt32
  var private1: UInt32
  var private2: UInt32
  var inSwipeGesture: UInt32
  var reserved: UInt32
  init()
  init(templateCacheIsInvalid templateCacheIsInvalid: UInt32, private1 private1: UInt32, private2 private2: UInt32, inSwipeGesture inSwipeGesture: UInt32, reserved reserved: UInt32)
}
protocol NSPageControllerDelegate : NSObjectProtocol {
  @available(OSX 10.8, *)
  @discardableResult
  optional func pageController(_ pageController: NSPageController, identifierFor object: AnyObject) -> String
  @available(OSX 10.8, *)
  @discardableResult
  optional func pageController(_ pageController: NSPageController, viewControllerForIdentifier identifier: String) -> NSViewController
  @available(OSX 10.8, *)
  @discardableResult
  optional func pageController(_ pageController: NSPageController, frameFor object: AnyObject) -> NSRect
  @available(OSX 10.8, *)
  optional func pageController(_ pageController: NSPageController, prepare viewController: NSViewController, with object: AnyObject)
  @available(OSX 10.8, *)
  optional func pageController(_ pageController: NSPageController, didTransitionTo object: AnyObject)
  @available(OSX 10.8, *)
  optional func pageControllerWillStartLiveTransition(_ pageController: NSPageController)
  @available(OSX 10.8, *)
  optional func pageControllerDidEndLiveTransition(_ pageController: NSPageController)
}
