
enum MDLMeshBufferType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case vertex
  case index
}
@available(iOS 9.0, *)
class MDLMeshBufferMap : NSObject {
  init(bytes bytes: UnsafeMutablePointer<Void>, deallocator deallocator: (() -> Void)? = nil)
  var bytes: UnsafeMutablePointer<Void> { get }
}
@available(iOS 9.0, *)
protocol MDLMeshBuffer : NSObjectProtocol, NSCopying {
  func fill(_ data: NSData, offset offset: Int)
  @discardableResult
  func map() -> MDLMeshBufferMap
  var length: Int { get }
  var allocator: MDLMeshBufferAllocator { get }
  var type: MDLMeshBufferType { get }
}
@available(iOS 9.0, *)
class MDLMeshBufferData : NSObject, MDLMeshBuffer {
  init(type type: MDLMeshBufferType, length length: Int)
  init(type type: MDLMeshBufferType, data data: NSData?)
  var data: NSData { get }
}
@available(iOS 9.0, *)
protocol MDLMeshBufferZone : NSObjectProtocol {
  var capacity: Int { get }
  var allocator: MDLMeshBufferAllocator { get }
}
@available(iOS 9.0, *)
protocol MDLMeshBufferAllocator : NSObjectProtocol {
  @discardableResult
  func newZone(_ capacity: Int) -> MDLMeshBufferZone
  @discardableResult
  func newZoneForBuffers(withSize sizes: [NSNumber], andType types: [NSNumber]) -> MDLMeshBufferZone
  @discardableResult
  func newBuffer(_ length: Int, type type: MDLMeshBufferType) -> MDLMeshBuffer
  @discardableResult
  func newBuffer(with data: NSData, type type: MDLMeshBufferType) -> MDLMeshBuffer
  @discardableResult
  func newBuffer(from zone: MDLMeshBufferZone?, length length: Int, type type: MDLMeshBufferType) -> MDLMeshBuffer?
  @discardableResult
  func newBuffer(from zone: MDLMeshBufferZone?, data data: NSData, type type: MDLMeshBufferType) -> MDLMeshBuffer?
}
class MDLMeshBufferDataAllocator : NSObject, MDLMeshBufferAllocator {
}
class MDLMeshBufferZoneDefault : NSObject, MDLMeshBufferZone {
}
