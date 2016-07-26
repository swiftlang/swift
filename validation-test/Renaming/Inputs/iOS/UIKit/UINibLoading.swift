
@available(iOS 3.0, *)
let UINibExternalObjects: String
extension NSBundle {
  @discardableResult
  func loadNibNamed(_ name: String!, owner owner: AnyObject!, options options: [NSObject : AnyObject]! = [:]) -> [AnyObject]!
}
extension NSObject {
  class func awakeFromNib()
  func awakeFromNib()
  @available(iOS 8.0, *)
  class func prepareForInterfaceBuilder()
  @available(iOS 8.0, *)
  func prepareForInterfaceBuilder()
}
