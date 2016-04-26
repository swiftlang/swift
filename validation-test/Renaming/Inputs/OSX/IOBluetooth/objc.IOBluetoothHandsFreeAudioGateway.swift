
@available(OSX 10.7, *)
class IOBluetoothHandsFreeAudioGateway : IOBluetoothHandsFree {
  @available(OSX 10.7, *)
  func createIndicator(_ indicatorName: String!, min minValue: Int32, max maxValue: Int32, currentValue currentValue: Int32)
  @available(OSX 10.7, *)
  func process(atCommand atCommand: String!)
  @available(OSX 10.7, *)
  func sendOKResponse()
  @available(OSX 10.7, *)
  func sendResponse(_ response: String!)
  @available(OSX 10.7, *)
  func sendResponse(_ response: String!, withOK withOK: Bool)
}
protocol IOBluetoothHandsFreeAudioGatewayDelegate {
  @available(OSX 10.7, *)
  optional func handsFree(_ device: IOBluetoothHandsFreeAudioGateway!, hangup hangup: NSNumber!)
  @available(OSX 10.7, *)
  optional func handsFree(_ device: IOBluetoothHandsFreeAudioGateway!, redial redial: NSNumber!)
}
