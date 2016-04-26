
protocol QLPreviewItem : NSObjectProtocol {
  var previewItemURL: NSURL { get }
  optional var previewItemTitle: String? { get }
}
extension NSURL : QLPreviewItem {
}
