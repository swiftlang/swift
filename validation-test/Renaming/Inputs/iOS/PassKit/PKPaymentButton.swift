
@available(iOS 8.3, *)
enum PKPaymentButtonStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case white
  case whiteOutline
  case black
}
@available(iOS 8.3, *)
enum PKPaymentButtonType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case plain
  case buy
  @available(iOS 9.0, *)
  case setUp
}
@available(iOS 8.3, *)
class PKPaymentButton : UIButton {
  convenience init(type buttonType: PKPaymentButtonType, style buttonStyle: PKPaymentButtonStyle)
  @available(iOS 9.0, *)
  init(paymentButtonType type: PKPaymentButtonType, paymentButtonStyle style: PKPaymentButtonStyle)
}
