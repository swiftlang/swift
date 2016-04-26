
struct UIEdgeInsets {
  var top: CGFloat
  var left: CGFloat
  var bottom: CGFloat
  var right: CGFloat
  init()
  init(top top: CGFloat, left left: CGFloat, bottom bottom: CGFloat, right right: CGFloat)
}

extension UIEdgeInsets {
  static var zero: UIEdgeInsets { get }
}

extension UIEdgeInsets : Equatable {
}
struct UIOffset {
  var horizontal: CGFloat
  var vertical: CGFloat
  init()
  init(horizontal horizontal: CGFloat, vertical vertical: CGFloat)
}

extension UIOffset {
  static var zero: UIOffset { get }
}

extension UIOffset : Equatable {
}
@available(watchOS 2.0, *)
struct UIRectEdge : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var top: UIRectEdge { get }
  static var left: UIRectEdge { get }
  static var bottom: UIRectEdge { get }
  static var right: UIRectEdge { get }
  static var all: UIRectEdge { get }
}
@discardableResult
func UIEdgeInsetsMake(_ top: CGFloat, _ left: CGFloat, _ bottom: CGFloat, _ right: CGFloat) -> UIEdgeInsets
@discardableResult
func UIEdgeInsetsInsetRect(_ rect: CGRect, _ insets: UIEdgeInsets) -> CGRect
@discardableResult
func UIOffsetMake(_ horizontal: CGFloat, _ vertical: CGFloat) -> UIOffset
@discardableResult
func UIEdgeInsetsEqualToEdgeInsets(_ insets1: UIEdgeInsets, _ insets2: UIEdgeInsets) -> Bool
@discardableResult
func UIOffsetEqualToOffset(_ offset1: UIOffset, _ offset2: UIOffset) -> Bool
let UIEdgeInsetsZero: UIEdgeInsets
let UIOffsetZero: UIOffset
@discardableResult
func NSStringFromCGPoint(_ point: CGPoint) -> String
@discardableResult
func NSStringFromCGVector(_ vector: CGVector) -> String
@discardableResult
func NSStringFromCGSize(_ size: CGSize) -> String
@discardableResult
func NSStringFromCGRect(_ rect: CGRect) -> String
@discardableResult
func NSStringFromCGAffineTransform(_ transform: CGAffineTransform) -> String
@discardableResult
func NSStringFromUIEdgeInsets(_ insets: UIEdgeInsets) -> String
@discardableResult
func NSStringFromUIOffset(_ offset: UIOffset) -> String
@discardableResult
func CGPointFromString(_ string: String) -> CGPoint
@discardableResult
func CGVectorFromString(_ string: String) -> CGVector
@discardableResult
func CGSizeFromString(_ string: String) -> CGSize
@discardableResult
func CGRectFromString(_ string: String) -> CGRect
@discardableResult
func CGAffineTransformFromString(_ string: String) -> CGAffineTransform
@discardableResult
func UIEdgeInsetsFromString(_ string: String) -> UIEdgeInsets
@discardableResult
func UIOffsetFromString(_ string: String) -> UIOffset
extension NSValue {
  /*not inherited*/ init(cgPoint point: CGPoint)
  /*not inherited*/ init(cgVector vector: CGVector)
  /*not inherited*/ init(cgSize size: CGSize)
  /*not inherited*/ init(cgRect rect: CGRect)
  /*not inherited*/ init(cgAffineTransform transform: CGAffineTransform)
  /*not inherited*/ init(uiEdgeInsets insets: UIEdgeInsets)
  @available(watchOS 2.0, *)
  /*not inherited*/ init(uiOffset insets: UIOffset)
  @discardableResult
  func cgPointValue() -> CGPoint
  @discardableResult
  func cgVectorValue() -> CGVector
  @discardableResult
  func cgSizeValue() -> CGSize
  @discardableResult
  func cgRectValue() -> CGRect
  @discardableResult
  func cgAffineTransform() -> CGAffineTransform
  @discardableResult
  func uiEdgeInsetsValue() -> UIEdgeInsets
  @available(watchOS 2.0, *)
  @discardableResult
  func uiOffsetValue() -> UIOffset
}
extension NSCoder {
  func encode(_ point: CGPoint, forKey key: String)
  func encode(_ vector: CGVector, forKey key: String)
  func encode(_ size: CGSize, forKey key: String)
  func encode(_ rect: CGRect, forKey key: String)
  func encode(_ transform: CGAffineTransform, forKey key: String)
  func encode(_ insets: UIEdgeInsets, forKey key: String)
  @available(watchOS 2.0, *)
  func encode(_ offset: UIOffset, forKey key: String)
  @discardableResult
  func decodeCGPoint(forKey key: String) -> CGPoint
  @discardableResult
  func decodeCGVector(forKey key: String) -> CGVector
  @discardableResult
  func decodeCGSize(forKey key: String) -> CGSize
  @discardableResult
  func decodeCGRect(forKey key: String) -> CGRect
  @discardableResult
  func decodeCGAffineTransform(forKey key: String) -> CGAffineTransform
  @discardableResult
  func decodeUIEdgeInsets(forKey key: String) -> UIEdgeInsets
  @available(watchOS 2.0, *)
  @discardableResult
  func decodeUIOffset(forKey key: String) -> UIOffset
}
