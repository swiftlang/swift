@_exported import Categories_A

extension X {
  public func fromOverlayForA() {}
  @objc public func fromOverlayForAObjC() {}

  @available(*, deprecated, message: "Categories_A.swift")
  public override func overriddenInOverlayForA() {}
}
