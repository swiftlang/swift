
typealias CGDirectDisplayID = UInt32
typealias CGOpenGLDisplayMask = UInt32
typealias CGRefreshRate = Double
class CGDisplayMode {
}
@available(OSX 10.2, *)
@discardableResult
func CGMainDisplayID() -> CGDirectDisplayID
@available(OSX 10.0, *)
@discardableResult
func CGGetDisplaysWithPoint(_ point: CGPoint, _ maxDisplays: UInt32, _ displays: UnsafeMutablePointer<CGDirectDisplayID>?, _ matchingDisplayCount: UnsafeMutablePointer<UInt32>?) -> CGError
@available(OSX 10.0, *)
@discardableResult
func CGGetDisplaysWithRect(_ rect: CGRect, _ maxDisplays: UInt32, _ displays: UnsafeMutablePointer<CGDirectDisplayID>?, _ matchingDisplayCount: UnsafeMutablePointer<UInt32>?) -> CGError
@available(OSX 10.0, *)
@discardableResult
func CGGetDisplaysWithOpenGLDisplayMask(_ mask: CGOpenGLDisplayMask, _ maxDisplays: UInt32, _ displays: UnsafeMutablePointer<CGDirectDisplayID>?, _ matchingDisplayCount: UnsafeMutablePointer<UInt32>?) -> CGError
@available(OSX 10.0, *)
@discardableResult
func CGGetActiveDisplayList(_ maxDisplays: UInt32, _ activeDisplays: UnsafeMutablePointer<CGDirectDisplayID>?, _ displayCount: UnsafeMutablePointer<UInt32>?) -> CGError
@available(OSX 10.2, *)
@discardableResult
func CGGetOnlineDisplayList(_ maxDisplays: UInt32, _ onlineDisplays: UnsafeMutablePointer<CGDirectDisplayID>?, _ displayCount: UnsafeMutablePointer<UInt32>?) -> CGError
@available(OSX 10.0, *)
@discardableResult
func CGDisplayIDToOpenGLDisplayMask(_ display: CGDirectDisplayID) -> CGOpenGLDisplayMask
@available(OSX 10.2, *)
@discardableResult
func CGOpenGLDisplayMaskToDisplayID(_ mask: CGOpenGLDisplayMask) -> CGDirectDisplayID
@available(OSX 10.0, *)
@discardableResult
func CGDisplayBounds(_ display: CGDirectDisplayID) -> CGRect
@available(OSX 10.0, *)
@discardableResult
func CGDisplayPixelsWide(_ display: CGDirectDisplayID) -> Int
@available(OSX 10.0, *)
@discardableResult
func CGDisplayPixelsHigh(_ display: CGDirectDisplayID) -> Int
@available(OSX 10.6, *)
@discardableResult
func CGDisplayCopyAllDisplayModes(_ display: CGDirectDisplayID, _ options: CFDictionary?) -> CFArray?
@available(OSX 10.8, *)
let kCGDisplayShowDuplicateLowResolutionModes: CFString
@available(OSX 10.6, *)
@discardableResult
func CGDisplayCopyDisplayMode(_ display: CGDirectDisplayID) -> CGDisplayMode?
@available(OSX 10.6, *)
@discardableResult
func CGDisplaySetDisplayMode(_ display: CGDirectDisplayID, _ mode: CGDisplayMode?, _ options: CFDictionary?) -> CGError
extension CGDisplayMode {
  @available(OSX 10.6, *)
  var width: Int { get }
  @available(OSX 10.6, *)
  var height: Int { get }
  @available(OSX, introduced: 10.6, deprecated: 10.11)
  @discardableResult
  func copyPixelEncoding() -> CFString?
  @available(OSX 10.6, *)
  var refreshRate: Double { get }
  @available(OSX 10.6, *)
  var ioFlags: UInt32 { get }
  @available(OSX 10.6, *)
  var ioDisplayModeID: Int32 { get }
  @available(OSX 10.6, *)
  @discardableResult
  func isUsableForDesktopGUI() -> Bool
  @available(OSX 10.6, *)
  class var typeID: CFTypeID { get }
  @available(OSX 10.8, *)
  var pixelWidth: Int { get }
  @available(OSX 10.8, *)
  var pixelHeight: Int { get }
}
typealias CGGammaValue = Float
@available(OSX 10.0, *)
@discardableResult
func CGSetDisplayTransferByFormula(_ display: CGDirectDisplayID, _ redMin: CGGammaValue, _ redMax: CGGammaValue, _ redGamma: CGGammaValue, _ greenMin: CGGammaValue, _ greenMax: CGGammaValue, _ greenGamma: CGGammaValue, _ blueMin: CGGammaValue, _ blueMax: CGGammaValue, _ blueGamma: CGGammaValue) -> CGError
@available(OSX 10.0, *)
@discardableResult
func CGGetDisplayTransferByFormula(_ display: CGDirectDisplayID, _ redMin: UnsafeMutablePointer<CGGammaValue>?, _ redMax: UnsafeMutablePointer<CGGammaValue>?, _ redGamma: UnsafeMutablePointer<CGGammaValue>?, _ greenMin: UnsafeMutablePointer<CGGammaValue>?, _ greenMax: UnsafeMutablePointer<CGGammaValue>?, _ greenGamma: UnsafeMutablePointer<CGGammaValue>?, _ blueMin: UnsafeMutablePointer<CGGammaValue>?, _ blueMax: UnsafeMutablePointer<CGGammaValue>?, _ blueGamma: UnsafeMutablePointer<CGGammaValue>?) -> CGError
@available(OSX 10.3, *)
@discardableResult
func CGDisplayGammaTableCapacity(_ display: CGDirectDisplayID) -> UInt32
@available(OSX 10.0, *)
@discardableResult
func CGSetDisplayTransferByTable(_ display: CGDirectDisplayID, _ tableSize: UInt32, _ redTable: UnsafePointer<CGGammaValue>?, _ greenTable: UnsafePointer<CGGammaValue>?, _ blueTable: UnsafePointer<CGGammaValue>?) -> CGError
@available(OSX 10.0, *)
@discardableResult
func CGGetDisplayTransferByTable(_ display: CGDirectDisplayID, _ capacity: UInt32, _ redTable: UnsafeMutablePointer<CGGammaValue>?, _ greenTable: UnsafeMutablePointer<CGGammaValue>?, _ blueTable: UnsafeMutablePointer<CGGammaValue>?, _ sampleCount: UnsafeMutablePointer<UInt32>?) -> CGError
@available(OSX 10.0, *)
@discardableResult
func CGSetDisplayTransferByByteTable(_ display: CGDirectDisplayID, _ tableSize: UInt32, _ redTable: UnsafePointer<UInt8>, _ greenTable: UnsafePointer<UInt8>, _ blueTable: UnsafePointer<UInt8>) -> CGError
@available(OSX 10.0, *)
func CGDisplayRestoreColorSyncSettings()
struct CGCaptureOptions : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  @available(*, deprecated)
  static var noFill: CGCaptureOptions { get }
}
@available(OSX 10.0, *)
@discardableResult
func CGDisplayCapture(_ display: CGDirectDisplayID) -> CGError
@available(OSX 10.3, *)
@discardableResult
func CGDisplayCaptureWithOptions(_ display: CGDirectDisplayID, _ options: CGCaptureOptions) -> CGError
@available(OSX 10.0, *)
@discardableResult
func CGDisplayRelease(_ display: CGDirectDisplayID) -> CGError
@available(OSX 10.0, *)
@discardableResult
func CGCaptureAllDisplays() -> CGError
@available(OSX 10.3, *)
@discardableResult
func CGCaptureAllDisplaysWithOptions(_ options: CGCaptureOptions) -> CGError
@available(OSX 10.0, *)
@discardableResult
func CGReleaseAllDisplays() -> CGError
@available(OSX 10.0, *)
@discardableResult
func CGShieldingWindowID(_ display: CGDirectDisplayID) -> UInt32
@available(OSX 10.0, *)
@discardableResult
func CGShieldingWindowLevel() -> Int32
@available(OSX 10.6, *)
@discardableResult
func CGDisplayCreateImage(_ displayID: CGDirectDisplayID) -> CGImage?
extension CGImage {
  @available(OSX 10.6, *)
  init?(displayForRectDisplay display: CGDirectDisplayID, rect rect: CGRect)
}
@available(OSX 10.0, *)
@discardableResult
func CGDisplayHideCursor(_ display: CGDirectDisplayID) -> CGError
@available(OSX 10.0, *)
@discardableResult
func CGDisplayShowCursor(_ display: CGDirectDisplayID) -> CGError
@available(OSX 10.0, *)
@discardableResult
func CGDisplayMoveCursorToPoint(_ display: CGDirectDisplayID, _ point: CGPoint) -> CGError
@available(OSX 10.0, *)
func CGGetLastMouseDelta(_ deltaX: UnsafeMutablePointer<Int32>?, _ deltaY: UnsafeMutablePointer<Int32>?)
@available(OSX 10.3, *)
@discardableResult
func CGDisplayGetDrawingContext(_ display: CGDirectDisplayID) -> CGContext?
var kCGDisplayWidth: String { get }
var kCGDisplayHeight: String { get }
var kCGDisplayMode: String { get }
var kCGDisplayBitsPerPixel: String { get }
var kCGDisplayBitsPerSample: String { get }
var kCGDisplaySamplesPerPixel: String { get }
var kCGDisplayRefreshRate: String { get }
var kCGDisplayModeUsableForDesktopGUI: String { get }
var kCGDisplayIOFlags: String { get }
var kCGDisplayBytesPerRow: String { get }
var kCGIODisplayModeID: String { get }
var kCGDisplayModeIsSafeForHardware: String { get }
var kCGDisplayModeIsInterlaced: String { get }
var kCGDisplayModeIsStretched: String { get }
var kCGDisplayModeIsTelevisionOutput: String { get }
typealias CGDisplayCount = UInt32
typealias CGDisplayErr = CGError
