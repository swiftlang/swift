
@available(OSX 10.8, *)
class SCNRenderer : NSObject, SCNSceneRenderer, SCNTechniqueSupport {
  convenience init(context context: CGLContextObj, options options: [NSObject : AnyObject]? = [:])
  @available(OSX 10.11, *)
  convenience init(device device: MTLDevice?, options options: [NSObject : AnyObject]? = [:])
  @available(OSX 10.10, *)
  func render(atTime time: CFTimeInterval)
  @available(OSX 10.11, *)
  func render(atTime time: CFTimeInterval, viewport viewport: CGRect, commandBuffer commandBuffer: MTLCommandBuffer, passDescriptor renderPassDescriptor: MTLRenderPassDescriptor)
  @available(OSX 10.10, *)
  var nextFrameTime: CFTimeInterval { get }
  @available(OSX, introduced: 10.8, deprecated: 10.11)
  func render()
}
