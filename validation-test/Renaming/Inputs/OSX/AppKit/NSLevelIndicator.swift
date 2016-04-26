
class NSLevelIndicator : NSControl {
  @available(OSX 10.10, *)
  var levelIndicatorStyle: NSLevelIndicatorStyle
  var minValue: Double
  var maxValue: Double
  var warningValue: Double
  var criticalValue: Double
  var tickMarkPosition: NSTickMarkPosition
  var numberOfTickMarks: Int
  var numberOfMajorTickMarks: Int
  @discardableResult
  func tickMarkValue(at index: Int) -> Double
  @discardableResult
  func rectOfTickMark(at index: Int) -> NSRect
}
