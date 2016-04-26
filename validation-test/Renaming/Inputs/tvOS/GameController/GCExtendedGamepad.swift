
@available(tvOS 7.0, *)
class GCExtendedGamepad : NSObject {
  weak var controller: @sil_weak GCController? { get }
  var valueChangedHandler: GCExtendedGamepadValueChangedHandler?
  @discardableResult
  func saveSnapshot() -> GCExtendedGamepadSnapshot
  var dpad: GCControllerDirectionPad { get }
  var buttonA: GCControllerButtonInput { get }
  var buttonB: GCControllerButtonInput { get }
  var buttonX: GCControllerButtonInput { get }
  var buttonY: GCControllerButtonInput { get }
  var leftThumbstick: GCControllerDirectionPad { get }
  var rightThumbstick: GCControllerDirectionPad { get }
  var leftShoulder: GCControllerButtonInput { get }
  var rightShoulder: GCControllerButtonInput { get }
  var leftTrigger: GCControllerButtonInput { get }
  var rightTrigger: GCControllerButtonInput { get }
}
typealias GCExtendedGamepadValueChangedHandler = (GCExtendedGamepad, GCControllerElement) -> Void
