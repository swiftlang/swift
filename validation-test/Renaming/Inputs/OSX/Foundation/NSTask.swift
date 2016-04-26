
@available(OSX 10.6, *)
enum NSTaskTerminationReason : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case exit
  case uncaughtSignal
}
class NSTask : NSObject {
  var launchPath: String?
  var arguments: [String]?
  var environment: [String : String]?
  var currentDirectoryPath: String
  var standardInput: AnyObject?
  var standardOutput: AnyObject?
  var standardError: AnyObject?
  func launch()
  func interrupt()
  func terminate()
  @discardableResult
  func suspend() -> Bool
  @discardableResult
  func resume() -> Bool
  var processIdentifier: Int32 { get }
  var isRunning: Bool { get }
  var terminationStatus: Int32 { get }
  @available(OSX 10.6, *)
  var terminationReason: NSTaskTerminationReason { get }
  @available(OSX 10.7, *)
  var terminationHandler: ((NSTask) -> Void)?
  @available(OSX 10.10, *)
  var qualityOfService: NSQualityOfService
}
extension NSTask {
  @discardableResult
  class func launchedTask(withLaunchPath path: String, arguments arguments: [String]) -> NSTask
  func waitUntilExit()
}
let NSTaskDidTerminateNotification: String
