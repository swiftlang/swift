
extension NSBundle {
  @available(OSX 10.8, *)
  @discardableResult
  func loadNibNamed(_ nibName: String, owner owner: AnyObject?, topLevel topLevelObjects: AutoreleasingUnsafeMutablePointer<NSArray>?) -> Bool
}
extension NSObject {
  class func awakeFromNib()
  func awakeFromNib()
  @available(OSX 10.10, *)
  class func prepareForInterfaceBuilder()
  @available(OSX 10.10, *)
  func prepareForInterfaceBuilder()
}
extension NSBundle {
}
