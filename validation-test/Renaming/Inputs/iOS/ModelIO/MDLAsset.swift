
@available(iOS 9.0, *)
class MDLAsset : NSObject, NSCopying, NSFastEnumeration {
  init(url URL: NSURL)
  init(url URL: NSURL, vertexDescriptor vertexDescriptor: MDLVertexDescriptor?, bufferAllocator bufferAllocator: MDLMeshBufferAllocator?)
  init(url URL: NSURL, vertexDescriptor vertexDescriptor: MDLVertexDescriptor?, bufferAllocator bufferAllocator: MDLMeshBufferAllocator?, preserveTopology preserveTopology: Bool, error error: NSErrorPointer)
  @discardableResult
  func export(to URL: NSURL) -> Bool
  func export(to URL: NSURL, error error: ()) throws
  @discardableResult
  class func canImportFileExtension(_ extension: String) -> Bool
  @discardableResult
  class func canExportFileExtension(_ extension: String) -> Bool
  @discardableResult
  func boundingBox(atTime time: NSTimeInterval) -> MDLAxisAlignedBoundingBox
  var boundingBox: MDLAxisAlignedBoundingBox { get }
  var frameInterval: NSTimeInterval
  var startTime: NSTimeInterval
  var endTime: NSTimeInterval
  var url: NSURL? { get }
  var bufferAllocator: MDLMeshBufferAllocator { get }
  var vertexDescriptor: MDLVertexDescriptor? { get }
  func add(_ object: MDLObject)
  func remove(_ object: MDLObject)
  var count: Int { get }
  subscript(_ index: Int) -> MDLObject? { get }
  @discardableResult
  func object(at index: Int) -> MDLObject
}
