@_exported import Categories_C

extension X {
  public func fromOverlayForC() {}
  @objc public func fromOverlayForCObjC() {}

  @available(*, deprecated, message: "Categories_C.swift")
  public override func overriddenInOverlayForC() {}
}

extension SubclassFromC {
  @available(*, deprecated, message: "Categories_C.swift")
  public override func overriddenInSubclassInOverlayForC() {}
}
