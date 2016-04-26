
class NSSpeechRecognizer : NSObject {
  func startListening()
  func stopListening()
  unowned(unsafe) var delegate: @sil_unmanaged NSSpeechRecognizerDelegate?
  var commands: [String]?
  var displayedCommandsTitle: String?
  var listensInForegroundOnly: Bool
  var blocksOtherRecognizers: Bool
}
protocol NSSpeechRecognizerDelegate : NSObjectProtocol {
  optional func speechRecognizer(_ sender: NSSpeechRecognizer, didRecognizeCommand command: String)
}
