
@available(OSX 10.11, *)
class GKComponent : NSObject, NSCopying {
  weak var entity: @sil_weak GKEntity? { get }
  func update(withDeltaTime seconds: NSTimeInterval)
}
@available(OSX 10.11, *)
class GKComponentSystem<ComponentType : GKComponent> : NSObject, NSFastEnumeration {
  var componentClass: AnyClass { get }
  var components: [ComponentType] { get }
  subscript(_ idx: Int) -> ComponentType { get }
  init(componentClass cls: AnyClass)
  func addComponent(_ component: ComponentType)
  func addComponent(with entity: GKEntity)
  func removeComponent(with entity: GKEntity)
  func removeComponent(_ component: ComponentType)
  func update(withDeltaTime seconds: NSTimeInterval)
  init()
}
