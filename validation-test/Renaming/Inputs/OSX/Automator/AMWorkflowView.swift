
class AMWorkflowView : NSView {
  var isEditable: Bool
  var workflowController: AMWorkflowController?
}
struct __AMWorkflowViewFlags {
  var ignoreSubviewFrameChanges: ObjCBool
  var editingEnabled: ObjCBool
  var reserved: Int
  init()
  init(ignoreSubviewFrameChanges ignoreSubviewFrameChanges: ObjCBool, editingEnabled editingEnabled: ObjCBool, reserved reserved: Int)
}
