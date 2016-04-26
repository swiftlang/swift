
let NSUndoCloseGroupingRunLoopOrdering: Int
@available(watchOS 2.0, *)
class NSUndoManager : NSObject {
  func beginUndoGrouping()
  func endUndoGrouping()
  var groupingLevel: Int { get }
  func disableUndoRegistration()
  func enableUndoRegistration()
  var isUndoRegistrationEnabled: Bool { get }
  var groupsByEvent: Bool
  var levelsOfUndo: Int
  var runLoopModes: [String]
  func undo()
  func redo()
  func undoNestedGroup()
  var canUndo: Bool { get }
  var canRedo: Bool { get }
  var isUndoing: Bool { get }
  var isRedoing: Bool { get }
  func removeAllActions()
  func removeAllActions(withTarget target: AnyObject)
  func registerUndo(withTarget target: AnyObject, selector selector: Selector, object anObject: AnyObject?)
  @discardableResult
  func prepare(withInvocationTarget target: AnyObject) -> AnyObject
  @available(watchOS 2.0, *)
  func __registerUndoWithTarget(_ target: AnyObject, handler undoHandler: (AnyObject) -> Void)
  @available(watchOS 2.0, *)
  func setActionIsDiscardable(_ discardable: Bool)
  @available(watchOS 2.0, *)
  var undoActionIsDiscardable: Bool { get }
  @available(watchOS 2.0, *)
  var redoActionIsDiscardable: Bool { get }
  var undoActionName: String { get }
  var redoActionName: String { get }
  func setActionName(_ actionName: String)
  var undoMenuItemTitle: String { get }
  var redoMenuItemTitle: String { get }
  @discardableResult
  func undoMenuTitle(forUndoActionName actionName: String) -> String
  @discardableResult
  func redoMenuTitle(forUndoActionName actionName: String) -> String
}

extension NSUndoManager {
  func registerUndoWithTarget<TargetType : AnyObject>(_ target: TargetType, handler handler: (TargetType) -> Void)
}
@available(watchOS 2.0, *)
let NSUndoManagerGroupIsDiscardableKey: String
@available(watchOS 2.0, *)
let NSUndoManagerCheckpointNotification: String
@available(watchOS 2.0, *)
let NSUndoManagerWillUndoChangeNotification: String
@available(watchOS 2.0, *)
let NSUndoManagerWillRedoChangeNotification: String
@available(watchOS 2.0, *)
let NSUndoManagerDidUndoChangeNotification: String
@available(watchOS 2.0, *)
let NSUndoManagerDidRedoChangeNotification: String
@available(watchOS 2.0, *)
let NSUndoManagerDidOpenUndoGroupNotification: String
@available(watchOS 2.0, *)
let NSUndoManagerWillCloseUndoGroupNotification: String
@available(watchOS 2.0, *)
let NSUndoManagerDidCloseUndoGroupNotification: String
