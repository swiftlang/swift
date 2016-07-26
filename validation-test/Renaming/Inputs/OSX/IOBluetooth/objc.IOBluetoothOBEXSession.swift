
class IOBluetoothOBEXSession : OBEXSession, IOBluetoothRFCOMMChannelDelegate {
  @discardableResult
  class func withSDPServiceRecord(_ inSDPServiceRecord: IOBluetoothSDPServiceRecord!) -> Self!
  @discardableResult
  class func withDevice(_ inDevice: IOBluetoothDevice!, channelID inRFCOMMChannelID: BluetoothRFCOMMChannelID) -> Self!
  @discardableResult
  class func withIncomingRFCOMMChannel(_ inChannel: IOBluetoothRFCOMMChannel!, eventSelector inEventSelector: Selector!, selectorTarget inEventSelectorTarget: AnyObject!, refCon inUserRefCon: UnsafeMutablePointer<Void>!) -> Self!
  init!(sdpServiceRecord inSDPServiceRecord: IOBluetoothSDPServiceRecord!)
  init!(device inDevice: IOBluetoothDevice!, channelID inChannelID: BluetoothRFCOMMChannelID)
  init!(incomingRFCOMMChannel inChannel: IOBluetoothRFCOMMChannel!, eventSelector inEventSelector: Selector!, selectorTarget inEventSelectorTarget: AnyObject!, refCon inUserRefCon: UnsafeMutablePointer<Void>!)
  @discardableResult
  func getRFCOMMChannel() -> IOBluetoothRFCOMMChannel!
  @discardableResult
  func getDevice() -> IOBluetoothDevice!
  @discardableResult
  func sendBufferTroughChannel() -> IOReturn
  func restartTransmission()
  @discardableResult
  func isSessionTargetAMac() -> Bool
  func setOpenTransportConnectionAsyncSelector(_ inSelector: Selector!, target inSelectorTarget: AnyObject!, refCon inUserRefCon: AnyObject!)
  func setOBEXSessionOpenConnectionCallback(_ inCallback: IOBluetoothOBEXSessionOpenConnectionCallback!, refCon inUserRefCon: UnsafeMutablePointer<Void>!)
}
