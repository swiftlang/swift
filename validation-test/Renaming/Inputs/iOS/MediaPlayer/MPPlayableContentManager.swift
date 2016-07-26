
@available(iOS 7.1, *)
class MPPlayableContentManager : NSObject {
  weak var dataSource: @sil_weak MPPlayableContentDataSource?
  weak var delegate: @sil_weak MPPlayableContentDelegate?
  @available(iOS 8.4, *)
  var context: MPPlayableContentManagerContext { get }
  @discardableResult
  class func shared() -> Self
  func reloadData()
  func beginUpdates()
  func endUpdates()
}
