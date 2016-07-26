
class NSStepper : NSControl, NSAccessibilityStepper {
  var minValue: Double
  var maxValue: Double
  var increment: Double
  var valueWraps: Bool
  var autorepeat: Bool
}
