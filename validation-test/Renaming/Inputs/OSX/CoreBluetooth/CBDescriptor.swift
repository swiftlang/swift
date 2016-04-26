
@available(OSX 10.7, *)
class CBDescriptor : NSObject {
  unowned(unsafe) var characteristic: @sil_unmanaged CBCharacteristic { get }
  var uuid: CBUUID { get }
  var value: AnyObject? { get }
}
@available(OSX 10.9, *)
class CBMutableDescriptor : CBDescriptor {
  init(type UUID: CBUUID, value value: AnyObject?)
}
