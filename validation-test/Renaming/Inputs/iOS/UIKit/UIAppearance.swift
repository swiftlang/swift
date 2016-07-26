
protocol UIAppearanceContainer : NSObjectProtocol {
}
protocol UIAppearance : NSObjectProtocol {
  @discardableResult
  static func appearance() -> Self
  @available(iOS 9.0, *)
  @discardableResult
  static func whenContained(inInstancesOfClasses containerTypes: [AnyObject.Type]) -> Self
  @available(iOS 8.0, *)
  @discardableResult
  static func forTraitCollection(_ trait: UITraitCollection) -> Self
  @available(iOS 9.0, *)
  @discardableResult
  static func forTraitCollection(_ trait: UITraitCollection, whenContainedInInstancesOfClasses containerTypes: [AnyObject.Type]) -> Self
}
