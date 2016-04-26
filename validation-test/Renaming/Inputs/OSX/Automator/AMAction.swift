
enum AMLogLevel : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case debug
  case info
  case warn
  case error
}
@available(OSX 10.4, *)
class AMAction : NSObject {
  init?(definition dict: [String : AnyObject], fromArchive archived: Bool)
  @available(OSX 10.5, *)
  init(contentsOf fileURL: NSURL) throws
  @available(OSX 10.5, *)
  var name: String { get }
  @available(OSX 10.5, *)
  var ignoresInput: Bool { get }
  @available(OSX 10.6, *)
  var selectedInputType: String?
  @available(OSX 10.6, *)
  var selectedOutputType: String?
  @available(OSX 10.6, *)
  var progressValue: CGFloat
  @available(OSX 10.7, *)
  @discardableResult
  func run(withInput input: AnyObject?) throws -> AnyObject
  @available(OSX 10.5, *)
  func runAsynchronously(withInput input: AnyObject?)
  @available(OSX 10.5, *)
  func willFinishRunning()
  @available(OSX 10.7, *)
  func finishRunningWithError(_ error: NSError?)
  @available(OSX 10.5, *)
  var output: AnyObject?
  func stop()
  func reset()
  func write(to dictionary: NSMutableDictionary)
  func opened()
  func activated()
  @available(OSX 10.5, *)
  func closed()
  func updateParameters()
  func parametersUpdated()
  var isStopped: Bool { get }
}
