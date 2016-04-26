
@available(iOS 8.0, *)
class WKWindowFeatures : NSObject {
  var menuBarVisibility: NSNumber? { get }
  var statusBarVisibility: NSNumber? { get }
  var toolbarsVisibility: NSNumber? { get }
  var allowsResizing: NSNumber? { get }
  var x: NSNumber? { get }
  var y: NSNumber? { get }
  var width: NSNumber? { get }
  var height: NSNumber? { get }
}
