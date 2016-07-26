
protocol AUCocoaUIBase {
  @discardableResult
  func interfaceVersion() -> UInt32
  @discardableResult
  func uiView(forAudioUnit inAudioUnit: AudioUnit, with inPreferredSize: NSSize) -> NSView?
}
