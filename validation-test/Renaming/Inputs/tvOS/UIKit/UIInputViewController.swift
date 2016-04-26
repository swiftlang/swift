
protocol UITextDocumentProxy : UIKeyInput {
  var documentContextBeforeInput: String? { get }
  var documentContextAfterInput: String? { get }
  func adjustTextPosition(byCharacterOffset offset: Int)
}
@available(tvOS 8.0, *)
class UIInputViewController : UIViewController, UITextInputDelegate {
  var textDocumentProxy: UITextDocumentProxy { get }
  var primaryLanguage: String?
  func dismissKeyboard()
  func advanceToNextInputMode()
  func requestSupplementaryLexicon(completion completionHandler: (UILexicon) -> Void)
}
