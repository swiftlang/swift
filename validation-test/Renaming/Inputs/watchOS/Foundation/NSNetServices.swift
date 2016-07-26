
let NSNetServicesErrorCode: String
let NSNetServicesErrorDomain: String
enum NSNetServicesError : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknownError
  case collisionError
  case notFoundError
  case activityInProgress
  case badArgumentError
  case cancelledError
  case invalidError
  case timeoutError
}
struct NSNetServiceOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var noAutoRename: NSNetServiceOptions { get }
  @available(watchOS 2.0, *)
  static var listenForConnections: NSNetServiceOptions { get }
}
