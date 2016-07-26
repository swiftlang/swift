
protocol PHContentEditingController : NSObjectProtocol {
  @available(iOS 8.0, *)
  @discardableResult
  func canHandle(_ adjustmentData: PHAdjustmentData!) -> Bool
  @available(iOS 8.0, *)
  func startContentEditing(with contentEditingInput: PHContentEditingInput!, placeholderImage placeholderImage: UIImage!)
  @available(iOS 8.0, *)
  func finishContentEditing(completionHandler completionHandler: ((PHContentEditingOutput!) -> Void)!)
  func cancelContentEditing()
  var shouldShowCancelConfirmation: Bool { get }
}
