
@available(tvOS 5.0, *)
class CBDescriptor : CBAttribute {
  unowned(unsafe) var characteristic: @sil_unmanaged CBCharacteristic { get }
  var value: AnyObject? { get }
}
@available(tvOS 6.0, *)
class CBMutableDescriptor : CBDescriptor {
}
