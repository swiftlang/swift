
@available(OSX 10.5, *)
class NSTreeNode : NSObject {
  init(representedObject modelObject: AnyObject?)
  var representedObject: AnyObject? { get }
  var indexPath: NSIndexPath { get }
  var isLeaf: Bool { get }
  var childNodes: [NSTreeNode]? { get }
  var mutableChildNodes: NSMutableArray { get }
  @discardableResult
  func descendantNode(at indexPath: NSIndexPath) -> NSTreeNode?
  unowned(unsafe) var parent: @sil_unmanaged NSTreeNode? { get }
  func sort(with sortDescriptors: [NSSortDescriptor], recursively recursively: Bool)
}
struct __NSTreeNodeFlags {
  var ignoreObserving: UInt32
  var reserved: UInt32
  init()
  init(ignoreObserving ignoreObserving: UInt32, reserved reserved: UInt32)
}
