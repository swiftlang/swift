
@available(iOS 4.0, *)
class CTCallCenter : NSObject {
  @available(iOS 4.0, *)
  var currentCalls: Set<CTCall>? { get }
  @available(iOS 4.0, *)
  var callEventHandler: ((CTCall) -> Void)?
}
