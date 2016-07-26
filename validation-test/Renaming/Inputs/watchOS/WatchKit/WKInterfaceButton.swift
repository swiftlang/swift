
@available(watchOS 2.0, *)
class WKInterfaceButton : WKInterfaceObject {
  func setTitle(_ title: String?)
  func setAttributedTitle(_ attributedTitle: NSAttributedString?)
  func setBackgroundColor(_ color: UIColor?)
  func setBackgroundImage(_ image: UIImage?)
  func setBackgroundImageData(_ imageData: NSData?)
  func setBackgroundImageNamed(_ imageName: String?)
  func setEnabled(_ enabled: Bool)
}
