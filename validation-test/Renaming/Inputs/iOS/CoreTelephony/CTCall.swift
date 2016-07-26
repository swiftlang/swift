
@available(iOS 4.0, *)
let CTCallStateDialing: String
@available(iOS 4.0, *)
let CTCallStateIncoming: String
@available(iOS 4.0, *)
let CTCallStateConnected: String
@available(iOS 4.0, *)
let CTCallStateDisconnected: String
@available(iOS 4.0, *)
class CTCall : NSObject {
  @available(iOS 4.0, *)
  var callState: String { get }
  @available(iOS 4.0, *)
  var callID: String { get }
}
