@_exported import Categories_B

extension X {
  public func fromOverlayForB() {}
  @objc public func fromOverlayForBObjC() {}

  @available(*, deprecated, message: "Categories_B.swift")
  public override func overriddenInOverlayForB() {}
}
