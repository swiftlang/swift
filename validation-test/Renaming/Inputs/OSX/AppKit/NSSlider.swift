
class NSSlider : NSControl, NSAccessibilitySlider {
  @available(OSX 10.10, *)
  var sliderType: NSSliderType
  var minValue: Double
  var maxValue: Double
  var altIncrementValue: Double
  var knobThickness: CGFloat { get }
  var vertical: Int { get }
}
extension NSSlider {
  var numberOfTickMarks: Int
  var tickMarkPosition: NSTickMarkPosition
  var allowsTickMarkValuesOnly: Bool
  @discardableResult
  func tickMarkValue(at index: Int) -> Double
  @discardableResult
  func rectOfTickMark(at index: Int) -> NSRect
  @discardableResult
  func indexOfTickMark(at point: NSPoint) -> Int
  @discardableResult
  func closestTickMarkValue(toValue value: Double) -> Double
}
