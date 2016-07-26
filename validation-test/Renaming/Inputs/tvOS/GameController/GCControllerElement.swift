
@available(tvOS 7.0, *)
class GCControllerElement : NSObject {
  weak var collection: @sil_weak GCControllerElement? { get }
  var isAnalog: Bool { get }
}
