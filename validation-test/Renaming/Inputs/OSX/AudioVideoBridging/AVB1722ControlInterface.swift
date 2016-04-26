
@available(OSX 10.8, *)
class AVB1722ControlInterface : NSObject {
  var interfaceName: String { get }
  unowned(unsafe) var interface: @sil_unmanaged AVBInterface? { get }
  @available(OSX 10.9, *)
  init?(interfaceName anInterfaceName: String)
  @available(OSX 10.11, *)
  init?(interface anInterface: AVBInterface)
}
