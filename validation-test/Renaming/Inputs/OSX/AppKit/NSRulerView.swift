
enum NSRulerOrientation : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case horizontalRuler
  case verticalRuler
}
class NSRulerView : NSView {
  class func registerUnit(withName unitName: String, abbreviation abbreviation: String, unitToPointsConversionFactor conversionFactor: CGFloat, stepUpCycle stepUpCycle: [NSNumber], stepDownCycle stepDownCycle: [NSNumber])
  init(scrollView scrollView: NSScrollView?, orientation orientation: NSRulerOrientation)
  unowned(unsafe) var scrollView: @sil_unmanaged NSScrollView?
  var orientation: NSRulerOrientation
  var baselineLocation: CGFloat { get }
  var requiredThickness: CGFloat { get }
  var ruleThickness: CGFloat
  var reservedThicknessForMarkers: CGFloat
  var reservedThicknessForAccessoryView: CGFloat
  var measurementUnits: String
  var originOffset: CGFloat
  unowned(unsafe) var clientView: @sil_unmanaged NSView?
  func addMarker(_ marker: NSRulerMarker)
  func removeMarker(_ marker: NSRulerMarker)
  var markers: [NSRulerMarker]?
  @discardableResult
  func trackMarker(_ marker: NSRulerMarker, withMouseEvent event: NSEvent) -> Bool
  var accessoryView: NSView?
  func moveRulerline(fromLocation oldLocation: CGFloat, toLocation newLocation: CGFloat)
  func invalidateHashMarks()
  func drawHashMarksAndLabels(in rect: NSRect)
  func drawMarkers(in rect: NSRect)
}
extension NSView {
  @discardableResult
  func rulerView(_ ruler: NSRulerView, shouldMove marker: NSRulerMarker) -> Bool
  @discardableResult
  func rulerView(_ ruler: NSRulerView, willMove marker: NSRulerMarker, toLocation location: CGFloat) -> CGFloat
  func rulerView(_ ruler: NSRulerView, didMove marker: NSRulerMarker)
  @discardableResult
  func rulerView(_ ruler: NSRulerView, shouldRemove marker: NSRulerMarker) -> Bool
  func rulerView(_ ruler: NSRulerView, didRemove marker: NSRulerMarker)
  @discardableResult
  func rulerView(_ ruler: NSRulerView, shouldAdd marker: NSRulerMarker) -> Bool
  @discardableResult
  func rulerView(_ ruler: NSRulerView, willAdd marker: NSRulerMarker, atLocation location: CGFloat) -> CGFloat
  func rulerView(_ ruler: NSRulerView, didAdd marker: NSRulerMarker)
  func rulerView(_ ruler: NSRulerView, handleMouseDown event: NSEvent)
  func rulerView(_ ruler: NSRulerView, willSetClientView newClient: NSView)
  @available(OSX 10.7, *)
  @discardableResult
  func rulerView(_ ruler: NSRulerView, locationFor aPoint: NSPoint) -> CGFloat
  @available(OSX 10.7, *)
  @discardableResult
  func rulerView(_ ruler: NSRulerView, pointForLocation aPoint: CGFloat) -> NSPoint
}
