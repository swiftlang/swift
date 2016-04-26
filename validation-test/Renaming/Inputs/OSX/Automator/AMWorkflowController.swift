
class AMWorkflowController : NSController {
  var workflow: AMWorkflow?
  var workflowView: AMWorkflowView?
  unowned(unsafe) var delegate: @sil_unmanaged AnyObject?
  var canRun: Bool { get }
  var isRunning: Bool { get }
  @IBAction func run(_ sender: AnyObject)
  @IBAction func stop(_ sender: AnyObject)
  var isPaused: Bool { get }
  @IBAction func pause(_ sender: AnyObject)
  @IBAction func step(_ sender: AnyObject)
  @IBAction func reset(_ sender: AnyObject)
}
struct __AMWorkflowControllerFlags {
  var shouldRunLocally: Int
  var isRunningLocally: Int
  var shouldDisplayProgressInMenuBar: Int
  var reserved: Int
  init()
  init(shouldRunLocally shouldRunLocally: Int, isRunningLocally isRunningLocally: Int, shouldDisplayProgressInMenuBar shouldDisplayProgressInMenuBar: Int, reserved reserved: Int)
}
struct __AMWorkflowControllerDelegateRespondTo {
  var workflowControllerDidAddWorkflow: Int
  var workflowControllerDidRemoveWorkflow: Int
  var workflowControllerWillRun: Int
  var workflowControllerWillStep: Int
  var workflowControllerWillStop: Int
  var workflowControllerWillPause: Int
  var workflowControllerDidRun: Int
  var workflowControllerDidStep: Int
  var workflowControllerDidStop: Int
  var workflowControllerDidPause: Int
  var workflowControllerWillRunAction: Int
  var workflowControllerDidRunAction: Int
  var workflowControllerDidError: Int
  var workflowControllerDidLogMessageOfTypeFromAction: Int
  var workflowControllerWillRunConversion: Int
  var workflowControllerDidRunConversion: Int
  var workflowControllerDidResumeWithAction: Int
  var reserved: Int
  init()
  init(workflowControllerDidAddWorkflow workflowControllerDidAddWorkflow: Int, workflowControllerDidRemoveWorkflow workflowControllerDidRemoveWorkflow: Int, workflowControllerWillRun workflowControllerWillRun: Int, workflowControllerWillStep workflowControllerWillStep: Int, workflowControllerWillStop workflowControllerWillStop: Int, workflowControllerWillPause workflowControllerWillPause: Int, workflowControllerDidRun workflowControllerDidRun: Int, workflowControllerDidStep workflowControllerDidStep: Int, workflowControllerDidStop workflowControllerDidStop: Int, workflowControllerDidPause workflowControllerDidPause: Int, workflowControllerWillRunAction workflowControllerWillRunAction: Int, workflowControllerDidRunAction workflowControllerDidRunAction: Int, workflowControllerDidError workflowControllerDidError: Int, workflowControllerDidLogMessageOfTypeFromAction workflowControllerDidLogMessageOfTypeFromAction: Int, workflowControllerWillRunConversion workflowControllerWillRunConversion: Int, workflowControllerDidRunConversion workflowControllerDidRunConversion: Int, workflowControllerDidResumeWithAction workflowControllerDidResumeWithAction: Int, reserved reserved: Int)
}
extension NSObject {
  class func workflowControllerWillRun(_ controller: AMWorkflowController)
  func workflowControllerWillRun(_ controller: AMWorkflowController)
  class func workflowControllerWillStop(_ controller: AMWorkflowController)
  func workflowControllerWillStop(_ controller: AMWorkflowController)
  class func workflowControllerDidRun(_ controller: AMWorkflowController)
  func workflowControllerDidRun(_ controller: AMWorkflowController)
  class func workflowControllerDidStop(_ controller: AMWorkflowController)
  func workflowControllerDidStop(_ controller: AMWorkflowController)
  class func workflowController(_ controller: AMWorkflowController, willRun action: AMAction)
  func workflowController(_ controller: AMWorkflowController, willRun action: AMAction)
  class func workflowController(_ controller: AMWorkflowController, didRun action: AMAction)
  func workflowController(_ controller: AMWorkflowController, didRun action: AMAction)
  class func workflowController(_ controller: AMWorkflowController, didError error: NSError)
  func workflowController(_ controller: AMWorkflowController, didError error: NSError)
}
