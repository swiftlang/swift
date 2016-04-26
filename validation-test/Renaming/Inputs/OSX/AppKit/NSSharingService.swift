
@available(OSX 10.8, *)
let NSSharingServiceNamePostOnFacebook: String
@available(OSX 10.8, *)
let NSSharingServiceNamePostOnTwitter: String
@available(OSX 10.8, *)
let NSSharingServiceNamePostOnSinaWeibo: String
@available(OSX 10.9, *)
let NSSharingServiceNamePostOnTencentWeibo: String
@available(OSX 10.9, *)
let NSSharingServiceNamePostOnLinkedIn: String
@available(OSX 10.8, *)
let NSSharingServiceNameComposeEmail: String
@available(OSX 10.8, *)
let NSSharingServiceNameComposeMessage: String
@available(OSX 10.8, *)
let NSSharingServiceNameSendViaAirDrop: String
@available(OSX 10.8, *)
let NSSharingServiceNameAddToSafariReadingList: String
@available(OSX 10.8, *)
let NSSharingServiceNameAddToIPhoto: String
@available(OSX 10.8, *)
let NSSharingServiceNameAddToAperture: String
@available(OSX 10.8, *)
let NSSharingServiceNameUseAsTwitterProfileImage: String
@available(OSX 10.9, *)
let NSSharingServiceNameUseAsFacebookProfileImage: String
@available(OSX 10.9, *)
let NSSharingServiceNameUseAsLinkedInProfileImage: String
@available(OSX 10.8, *)
let NSSharingServiceNameUseAsDesktopPicture: String
@available(OSX 10.8, *)
let NSSharingServiceNamePostImageOnFlickr: String
@available(OSX 10.8, *)
let NSSharingServiceNamePostVideoOnVimeo: String
@available(OSX 10.8, *)
let NSSharingServiceNamePostVideoOnYouku: String
@available(OSX 10.8, *)
let NSSharingServiceNamePostVideoOnTudou: String
@available(OSX 10.8, *)
class NSSharingService : NSObject {
  unowned(unsafe) var delegate: @sil_unmanaged NSSharingServiceDelegate?
  var title: String { get }
  var image: NSImage { get }
  var alternateImage: NSImage? { get }
  @available(OSX 10.9, *)
  var menuItemTitle: String
  @available(OSX 10.9, *)
  var recipients: [String]?
  @available(OSX 10.9, *)
  var subject: String?
  @available(OSX 10.9, *)
  var messageBody: String? { get }
  @available(OSX 10.9, *)
  @NSCopying var permanentLink: NSURL? { get }
  @available(OSX 10.9, *)
  var accountName: String? { get }
  @available(OSX 10.9, *)
  var attachmentFileURLs: [NSURL]? { get }
  @discardableResult
  class func sharingServices(forItems items: [AnyObject]) -> [NSSharingService]
  /*not inherited*/ init?(named serviceName: String)
  init(title title: String, image image: NSImage, alternateImage alternateImage: NSImage?, handler block: () -> Void)
  @discardableResult
  func canPerform(withItems items: [AnyObject]?) -> Bool
  func perform(withItems items: [AnyObject])
}
@available(OSX 10.8, *)
enum NSSharingContentScope : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case item
  case partial
  case full
}
protocol NSSharingServiceDelegate : NSObjectProtocol {
  @available(OSX 10.8, *)
  optional func sharingService(_ sharingService: NSSharingService, willShareItems items: [AnyObject])
  @available(OSX 10.8, *)
  optional func sharingService(_ sharingService: NSSharingService, didFailToShareItems items: [AnyObject], error error: NSError)
  @available(OSX 10.8, *)
  optional func sharingService(_ sharingService: NSSharingService, didShareItems items: [AnyObject])
  @available(OSX 10.8, *)
  @discardableResult
  optional func sharingService(_ sharingService: NSSharingService, sourceFrameOnScreenForShareItem item: AnyObject) -> NSRect
  @available(OSX 10.8, *)
  @discardableResult
  optional func sharingService(_ sharingService: NSSharingService, transitionImageForShareItem item: AnyObject, contentRect contentRect: UnsafeMutablePointer<NSRect>) -> NSImage
  @available(OSX 10.8, *)
  @discardableResult
  optional func sharingService(_ sharingService: NSSharingService, sourceWindowForShareItems items: [AnyObject], sharingContentScope sharingContentScope: UnsafeMutablePointer<NSSharingContentScope>) -> NSWindow?
}
@available(OSX 10.8, *)
class NSSharingServicePicker : NSObject {
  unowned(unsafe) var delegate: @sil_unmanaged NSSharingServicePickerDelegate?
  init(items items: [AnyObject])
  func showRelative(to rect: NSRect, of view: NSView, preferredEdge preferredEdge: NSRectEdge)
}
protocol NSSharingServicePickerDelegate : NSObjectProtocol {
  @available(OSX 10.8, *)
  @discardableResult
  optional func sharingServicePicker(_ sharingServicePicker: NSSharingServicePicker, sharingServicesForItems items: [AnyObject], proposedSharingServices proposedServices: [NSSharingService]) -> [NSSharingService]
  @available(OSX 10.8, *)
  @discardableResult
  optional func sharingServicePicker(_ sharingServicePicker: NSSharingServicePicker, delegateFor sharingService: NSSharingService) -> NSSharingServiceDelegate?
  @available(OSX 10.8, *)
  optional func sharingServicePicker(_ sharingServicePicker: NSSharingServicePicker, didChoose service: NSSharingService?)
}
