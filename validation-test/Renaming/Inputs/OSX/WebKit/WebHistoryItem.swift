
let WebHistoryItemChangedNotification: String
class WebHistoryItem : NSObject, NSCopying {
  init!(urlString URLString: String!, title title: String!, lastVisitedTimeInterval time: NSTimeInterval)
  var originalURLString: String! { get }
  var urlString: String! { get }
  var title: String! { get }
  var lastVisitedTimeInterval: NSTimeInterval { get }
  var alternateTitle: String!
  var icon: NSImage! { get }
}
