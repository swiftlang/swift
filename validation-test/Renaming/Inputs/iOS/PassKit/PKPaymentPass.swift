
@available(iOS 8.0, *)
enum PKPaymentPassActivationState : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case activated
  case requiresActivation
  case activating
  case suspended
  case deactivated
}
@available(iOS 8.0, *)
class PKPaymentPass : PKPass {
  var primaryAccountIdentifier: String { get }
  var primaryAccountNumberSuffix: String { get }
  var deviceAccountIdentifier: String { get }
  var deviceAccountNumberSuffix: String { get }
  var activationState: PKPaymentPassActivationState { get }
}
