
protocol NSValidatedUserInterfaceItem {
  @discardableResult
  func action() -> Selector?
  @discardableResult
  func tag() -> Int
}
protocol NSUserInterfaceValidations {
  @discardableResult
  func validate(_ anItem: NSValidatedUserInterfaceItem) -> Bool
}
