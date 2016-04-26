
protocol NSColorPickingDefault {
  init?(pickerMask mask: Int, colorPanel owningColorPanel: NSColorPanel)
  @discardableResult
  func provideNewButtonImage() -> NSImage
  func insertNewButtonImage(_ newButtonImage: NSImage, in buttonCell: NSButtonCell)
  func viewSizeChanged(_ sender: AnyObject?)
  func alphaControlAddedOrRemoved(_ sender: AnyObject?)
  func attach(_ colorList: NSColorList)
  func detach(_ colorList: NSColorList)
  func setMode(_ mode: NSColorPanelMode)
  @available(OSX 10.5, *)
  @discardableResult
  func buttonToolTip() -> String
  @available(OSX 10.5, *)
  @discardableResult
  func minContentSize() -> NSSize
}
protocol NSColorPickingCustom : NSColorPickingDefault {
  @discardableResult
  func supportsMode(_ mode: NSColorPanelMode) -> Bool
  @discardableResult
  func currentMode() -> NSColorPanelMode
  @discardableResult
  func provideNewView(_ initialRequest: Bool) -> NSView
  func setColor(_ newColor: NSColor)
}
