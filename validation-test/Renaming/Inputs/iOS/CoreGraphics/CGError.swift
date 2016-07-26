
enum CGError : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case success
  case failure
  case illegalArgument
  case invalidConnection
  case invalidContext
  case cannotComplete
  case notImplemented
  case rangeCheck
  case typeCheck
  case invalidOperation
  case noneAvailable
}
