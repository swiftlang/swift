
protocol UIAppearanceContainer : NSObjectProtocol {
}
protocol UIAppearance : NSObjectProtocol {
  @discardableResult
  static func appearance() -> Self
  @available(tvOS 9.0, *)
  @discardableResult
  static func whenContained(inInstancesOfClasses containerTypes: [AnyObject.Type]) -> Self
  @available(tvOS 8.0, *)
  @discardableResult
  static func forTraitCollection(_ trait: UITraitCollection) -> Self
  @available(tvOS 9.0, *)
  @discardableResult
  static func forTraitCollection(_ trait: UITraitCollection, whenContainedInInstancesOfClasses containerTypes: [AnyObject.Type]) -> Self
}
