
@available(iOS 9.0, *)
enum UIStackViewDistribution : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case fill
  case fillEqually
  case fillProportionally
  case equalSpacing
  case equalCentering
}
@available(iOS 9.0, *)
enum UIStackViewAlignment : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case fill
  case leading
  static var top: UIStackViewAlignment { get }
  case firstBaseline
  case center
  case trailing
  static var bottom: UIStackViewAlignment { get }
  case lastBaseline
}
@available(iOS 9.0, *)
class UIStackView : UIView {
  init(arrangedSubviews views: [UIView])
  var arrangedSubviews: [UIView] { get }
  func addArrangedSubview(_ view: UIView)
  func removeArrangedSubview(_ view: UIView)
  func insertArrangedSubview(_ view: UIView, at stackIndex: Int)
  var axis: UILayoutConstraintAxis
  var distribution: UIStackViewDistribution
  var alignment: UIStackViewAlignment
  var spacing: CGFloat
  var isBaselineRelativeArrangement: Bool
  var isLayoutMarginsRelativeArrangement: Bool
}
