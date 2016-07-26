
enum OSAScriptState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case stopped
  case running
  case recording
}
class OSAScriptController : NSController {
  var scriptView: OSAScriptView?
  var resultView: NSTextView?
  @NSCopying var script: OSAScript?
  var language: OSALanguage?
  var scriptState: OSAScriptState { get }
  var isCompiling: Bool { get }
  @IBAction func compileScript(_ sender: AnyObject?)
  @IBAction func recordScript(_ sender: AnyObject?)
  @IBAction func runScript(_ sender: AnyObject?)
  @IBAction func stopScript(_ sender: AnyObject?)
}
