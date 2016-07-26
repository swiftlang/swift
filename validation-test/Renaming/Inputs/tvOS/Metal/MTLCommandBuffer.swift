
@available(tvOS 8.0, *)
enum MTLCommandBufferStatus : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case notEnqueued
  case enqueued
  case committed
  case scheduled
  case completed
  case error
}
@available(tvOS 8.0, *)
let MTLCommandBufferErrorDomain: String
@available(tvOS 8.0, *)
enum MTLCommandBufferError : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case none
  case `internal`
  case timeout
  case pageFault
  case blacklisted
  case notPermitted
  case outOfMemory
  case invalidResource
}
typealias MTLCommandBufferHandler = (MTLCommandBuffer) -> Void
@available(tvOS 8.0, *)
protocol MTLCommandBuffer : NSObjectProtocol {
  var device: MTLDevice { get }
  var commandQueue: MTLCommandQueue { get }
  var retainedReferences: Bool { get }
  var label: String? { get set }
  func enqueue()
  func commit()
  func addScheduledHandler(_ block: MTLCommandBufferHandler)
  func present(_ drawable: MTLDrawable)
  func present(_ drawable: MTLDrawable, atTime presentationTime: CFTimeInterval)
  func waitUntilScheduled()
  func addCompletedHandler(_ block: MTLCommandBufferHandler)
  func waitUntilCompleted()
  var status: MTLCommandBufferStatus { get }
  var error: NSError? { get }
  @discardableResult
  func blitCommandEncoder() -> MTLBlitCommandEncoder
  @discardableResult
  func renderCommandEncoder(with renderPassDescriptor: MTLRenderPassDescriptor) -> MTLRenderCommandEncoder
  @discardableResult
  func computeCommandEncoder() -> MTLComputeCommandEncoder
  @discardableResult
  func parallelRenderCommandEncoder(with renderPassDescriptor: MTLRenderPassDescriptor) -> MTLParallelRenderCommandEncoder
}
