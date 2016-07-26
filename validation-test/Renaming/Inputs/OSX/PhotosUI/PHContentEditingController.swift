
protocol PHContentEditingController : NSObjectProtocol {
  func cancelContentEditing()
  var shouldShowCancelConfirmation: Bool { get }
}
