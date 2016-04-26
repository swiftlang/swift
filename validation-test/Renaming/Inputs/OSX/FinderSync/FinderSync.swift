
class FIFinderSyncController : NSExtensionContext {
  @discardableResult
  class func defaultController() -> Self
  var directoryURLs: Set<NSURL>!
  func setBadgeImage(_ image: NSImage, label label: String?, forBadgeIdentifier badgeID: String)
  func setBadgeIdentifier(_ badgeID: String, for url: NSURL)
  @discardableResult
  func targetedURL() -> NSURL?
  @discardableResult
  func selectedItemURLs() -> [NSURL]?
}
enum FIMenuKind : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case contextualMenuForItems
  case contextualMenuForContainer
  case contextualMenuForSidebar
  case toolbarItemMenu
}
protocol FIFinderSyncProtocol {
  @discardableResult
  optional func menu(for menu: FIMenuKind) -> NSMenu?
  optional func beginObservingDirectory(at url: NSURL)
  optional func endObservingDirectory(at url: NSURL)
  optional func requestBadgeIdentifier(for url: NSURL)
  optional var toolbarItemName: String { get }
  @NSCopying optional var toolbarItemImage: NSImage { get }
  optional var toolbarItemToolTip: String { get }
}
class FIFinderSync : NSObject, FIFinderSyncProtocol, NSExtensionRequestHandling {
}
