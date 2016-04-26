
struct UIPopoverArrowDirection : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var up: UIPopoverArrowDirection { get }
  static var down: UIPopoverArrowDirection { get }
  static var left: UIPopoverArrowDirection { get }
  static var right: UIPopoverArrowDirection { get }
  static var any: UIPopoverArrowDirection { get }
  static var unknown: UIPopoverArrowDirection { get }
}
extension UIViewController {
  @available(tvOS 3.2, *)
  var isModalInPopover: Bool
}
