
@available(tvOS 9.0, *)
class UIPressesEvent : UIEvent {
  @discardableResult
  func allPresses() -> Set<UIPress>
  @discardableResult
  func presses(for gesture: UIGestureRecognizer) -> Set<UIPress>
}
