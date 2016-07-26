
@available(tvOS 9.0, *)
class GKEntity : NSObject, NSCopying {
  func update(withDeltaTime seconds: NSTimeInterval)
  var components: [GKComponent] { get }
  func addComponent(_ component: GKComponent)
  func removeComponent(for componentClass: AnyClass)
}

@available(iOS 9.0, OSX 10.11, tvOS 9.0, *)
extension GKEntity {
  @warn_unused_result
  func componentForClass<ComponentType : GKComponent>(_ componentClass: ComponentType.Type) -> ComponentType?
}
