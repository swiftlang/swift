
@available(watchOS 2.0, *)
class WKInterfaceSwitch : WKInterfaceObject {
  func setTitle(_ title: String?)
  func setAttributedTitle(_ attributedTitle: NSAttributedString?)
  func setEnabled(_ enabled: Bool)
  func setOn(_ on: Bool)
  func setColor(_ color: UIColor?)
}
