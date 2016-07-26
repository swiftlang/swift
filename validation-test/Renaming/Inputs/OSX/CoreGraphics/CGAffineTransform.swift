
struct CGAffineTransform {
  var a: CGFloat
  var b: CGFloat
  var c: CGFloat
  var d: CGFloat
  var tx: CGFloat
  var ty: CGFloat
  init()
  init(a a: CGFloat, b b: CGFloat, c c: CGFloat, d d: CGFloat, tx tx: CGFloat, ty ty: CGFloat)
}

extension CGAffineTransform {
  static var identity: CGAffineTransform { get }
}
extension CGAffineTransform {
  @available(OSX 10.0, *)
  /*not inherited*/ init(withTranslationX tx: CGFloat, y ty: CGFloat)
  @available(OSX 10.0, *)
  /*not inherited*/ init(withScaleX sx: CGFloat, y sy: CGFloat)
  @available(OSX 10.0, *)
  /*not inherited*/ init(withRotationAngle angle: CGFloat)
  @available(OSX 10.4, *)
  @discardableResult
  func isIdentity() -> Bool
  @available(OSX 10.0, *)
  @discardableResult
  func translateBy(x tx: CGFloat, y ty: CGFloat) -> CGAffineTransform
  @available(OSX 10.0, *)
  @discardableResult
  func scaleBy(x sx: CGFloat, y sy: CGFloat) -> CGAffineTransform
  @available(OSX 10.0, *)
  @discardableResult
  func rotate(byAngle angle: CGFloat) -> CGAffineTransform
  @available(OSX 10.0, *)
  @discardableResult
  func invert() -> CGAffineTransform
  @available(OSX 10.0, *)
  @discardableResult
  func concat(_ t2: CGAffineTransform) -> CGAffineTransform
  @available(OSX 10.4, *)
  @discardableResult
  func equalTo(_ t2: CGAffineTransform) -> Bool
}
extension CGPoint {
  @available(OSX 10.0, *)
  @discardableResult
  func applyAffineTransform(_ t: CGAffineTransform) -> CGPoint
}
extension CGSize {
  @available(OSX 10.0, *)
  @discardableResult
  func applyAffineTransform(_ t: CGAffineTransform) -> CGSize
}
extension CGRect {
  @available(OSX 10.4, *)
  @discardableResult
  func applyAffineTransform(_ t: CGAffineTransform) -> CGRect
}
@discardableResult
func __CGAffineTransformMake(_ a: CGFloat, _ b: CGFloat, _ c: CGFloat, _ d: CGFloat, _ tx: CGFloat, _ ty: CGFloat) -> CGAffineTransform
@discardableResult
func __CGPointApplyAffineTransform(_ point: CGPoint, _ t: CGAffineTransform) -> CGPoint
@discardableResult
func __CGSizeApplyAffineTransform(_ size: CGSize, _ t: CGAffineTransform) -> CGSize
