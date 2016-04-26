
@available(iOS 8.2, *)
class WKInterfaceGroup : WKInterfaceObject, WKImageAnimatable {
  func setCornerRadius(_ cornerRadius: CGFloat)
  func setBackgroundColor(_ color: UIColor?)
  func setBackgroundImage(_ image: UIImage?)
  func setBackgroundImageData(_ imageData: NSData?)
  func setBackgroundImageNamed(_ imageName: String?)
}
