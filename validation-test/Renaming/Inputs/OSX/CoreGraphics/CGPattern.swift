
class CGPattern {
}
enum CGPatternTiling : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case noDistortion
  case constantSpacingMinimalDistortion
  case constantSpacing
}
typealias CGPatternDrawPatternCallback = @convention(c) (UnsafeMutablePointer<Void>?, CGContext?) -> Void
typealias CGPatternReleaseInfoCallback = @convention(c) (UnsafeMutablePointer<Void>?) -> Void
struct CGPatternCallbacks {
  var version: UInt32
  var drawPattern: CGPatternDrawPatternCallback?
  var releaseInfo: CGPatternReleaseInfoCallback?
  init()
  init(version version: UInt32, drawPattern drawPattern: CGPatternDrawPatternCallback?, releaseInfo releaseInfo: CGPatternReleaseInfoCallback?)
}
extension CGPattern {
  @available(OSX 10.2, *)
  class var typeID: CFTypeID { get }
  @available(OSX 10.0, *)
  init?(info info: UnsafeMutablePointer<Void>?, bounds bounds: CGRect, matrix matrix: CGAffineTransform, xStep xStep: CGFloat, yStep yStep: CGFloat, tiling tiling: CGPatternTiling, isColored isColored: Bool, callbacks callbacks: UnsafePointer<CGPatternCallbacks>?)
}
