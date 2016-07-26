
@available(watchOS 2.0, *)
class WKInterfaceSlider : WKInterfaceObject {
  func setEnabled(_ enabled: Bool)
  func setValue(_ value: Float)
  func setColor(_ color: UIColor?)
  func setNumberOfSteps(_ numberOfSteps: Int)
}
