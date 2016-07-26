
@available(iOS 5.0, *)
class CBDescriptor : CBAttribute {
  unowned(unsafe) var characteristic: @sil_unmanaged CBCharacteristic { get }
  var value: AnyObject? { get }
}
@available(iOS 6.0, *)
class CBMutableDescriptor : CBDescriptor {
  init(type UUID: CBUUID, value value: AnyObject?)
}
