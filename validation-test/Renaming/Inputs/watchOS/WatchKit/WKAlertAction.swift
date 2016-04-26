
@available(watchOS 2.0, *)
enum WKAlertActionStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case `default`
  case cancel
  case destructive
}
@available(watchOS 2.0, *)
typealias WKAlertActionHandler = () -> Void
@available(watchOS 2.0, *)
class WKAlertAction : NSObject {
  convenience init(title title: String, style style: WKAlertActionStyle, handler handler: WKAlertActionHandler)
}
