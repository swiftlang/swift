
@available(tvOS 3.0, *)
let UINibExternalObjects: String
extension NSBundle {
  @discardableResult
  func loadNibNamed(_ name: String!, owner owner: AnyObject!, options options: [NSObject : AnyObject]! = [:]) -> [AnyObject]!
}
extension NSObject {
  class func awakeFromNib()
  func awakeFromNib()
  @available(tvOS 8.0, *)
  class func prepareForInterfaceBuilder()
  @available(tvOS 8.0, *)
  func prepareForInterfaceBuilder()
}
