
class IOBluetoothDevicePair : NSObject {
  unowned(unsafe) var delegate: @sil_unmanaged AnyObject!
  convenience init!(device device: IOBluetoothDevice!)
  @discardableResult
  func start() -> IOReturn
  func stop()
  @discardableResult
  func device() -> IOBluetoothDevice!
  func setDevice(_ inDevice: IOBluetoothDevice!)
  func replyPINCode(_ PINCodeSize: Int, pinCode PINCode: UnsafeMutablePointer<BluetoothPINCode>!)
  func replyUserConfirmation(_ reply: Bool)
}
protocol IOBluetoothDevicePairDelegate : NSObjectProtocol {
  optional func devicePairingStarted(_ sender: AnyObject!)
  optional func devicePairingConnecting(_ sender: AnyObject!)
  optional func devicePairingPINCodeRequest(_ sender: AnyObject!)
  optional func devicePairingUserConfirmationRequest(_ sender: AnyObject!, numericValue numericValue: BluetoothNumericValue)
  optional func devicePairingUserPasskeyNotification(_ sender: AnyObject!, passkey passkey: BluetoothPasskey)
  optional func devicePairingFinished(_ sender: AnyObject!, error error: IOReturn)
  optional func deviceSimplePairingComplete(_ sender: AnyObject!, status status: BluetoothHCIEventStatus)
}
