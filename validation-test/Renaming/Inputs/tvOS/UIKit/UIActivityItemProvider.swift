
protocol UIActivityItemSource : NSObjectProtocol {
  @discardableResult
  func activityViewControllerPlaceholderItem(_ activityViewController: UIActivityViewController) -> AnyObject
  @discardableResult
  func activityViewController(_ activityViewController: UIActivityViewController, itemForActivityType activityType: String) -> AnyObject?
  @discardableResult
  optional func activityViewController(_ activityViewController: UIActivityViewController, subjectForActivityType activityType: String?) -> String
  @discardableResult
  optional func activityViewController(_ activityViewController: UIActivityViewController, dataTypeIdentifierForActivityType activityType: String?) -> String
  @available(tvOS 2.0, *)
  @discardableResult
  optional func activityViewController(_ activityViewController: UIActivityViewController, thumbnailImageForActivityType activityType: String?, suggestedSize size: CGSize) -> UIImage?
}
