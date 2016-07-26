
@available(iOS 8.0, *)
class SCNRenderer : NSObject, SCNSceneRenderer, SCNTechniqueSupport {
  convenience init(context context: EAGLContext, options options: [NSObject : AnyObject]? = [:])
  @available(iOS 9.0, *)
  convenience init(device device: MTLDevice?, options options: [NSObject : AnyObject]? = [:])
  @available(iOS 8.0, *)
  func render(atTime time: CFTimeInterval)
  @available(iOS 9.0, *)
  func render(atTime time: CFTimeInterval, viewport viewport: CGRect, commandBuffer commandBuffer: MTLCommandBuffer, passDescriptor renderPassDescriptor: MTLRenderPassDescriptor)
  @available(iOS 8.0, *)
  var nextFrameTime: CFTimeInterval { get }
  @available(iOS, introduced: 8.0, deprecated: 9.0)
  func render()
}
