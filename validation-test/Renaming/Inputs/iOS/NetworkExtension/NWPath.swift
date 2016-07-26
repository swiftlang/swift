
@available(iOS 9.0, *)
enum NWPathStatus : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case invalid
  case satisfied
  case unsatisfied
  case satisfiable
}
@available(iOS 9.0, *)
class NWPath : NSObject {
  @available(iOS 9.0, *)
  var status: NWPathStatus { get }
  @available(iOS 9.0, *)
  var isExpensive: Bool { get }
  @available(iOS 9.0, *)
  @discardableResult
  func isEqual(to path: NWPath) -> Bool
}
