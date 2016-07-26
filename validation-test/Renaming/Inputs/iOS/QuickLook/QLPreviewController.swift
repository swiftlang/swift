
@available(iOS 4.0, *)
class QLPreviewController : UIViewController {
  @discardableResult
  class func canPreviewItem(_ item: QLPreviewItem) -> Bool
  weak var dataSource: @sil_weak QLPreviewControllerDataSource?
  func reloadData()
  func refreshCurrentPreviewItem()
  var currentPreviewItemIndex: Int
  var currentPreviewItem: QLPreviewItem? { get }
  weak var delegate: @sil_weak QLPreviewControllerDelegate?
}
protocol QLPreviewControllerDataSource {
  @available(iOS 4.0, *)
  @discardableResult
  func numberOfPreviewItems(in controller: QLPreviewController) -> Int
  @available(iOS 4.0, *)
  @discardableResult
  func previewController(_ controller: QLPreviewController, previewItemAt index: Int) -> QLPreviewItem
}
protocol QLPreviewControllerDelegate : NSObjectProtocol {
  @available(iOS 4.0, *)
  optional func previewControllerWillDismiss(_ controller: QLPreviewController)
  @available(iOS 4.0, *)
  optional func previewControllerDidDismiss(_ controller: QLPreviewController)
  @available(iOS 4.0, *)
  @discardableResult
  optional func previewController(_ controller: QLPreviewController, shouldOpen url: NSURL, for item: QLPreviewItem) -> Bool
  @available(iOS 4.0, *)
  @discardableResult
  optional func previewController(_ controller: QLPreviewController, frameFor item: QLPreviewItem, inSourceView view: AutoreleasingUnsafeMutablePointer<UIView?>) -> CGRect
  @available(iOS 4.0, *)
  @discardableResult
  optional func previewController(_ controller: QLPreviewController, transitionImageFor item: QLPreviewItem, contentRect contentRect: UnsafeMutablePointer<CGRect>) -> UIImage
}
