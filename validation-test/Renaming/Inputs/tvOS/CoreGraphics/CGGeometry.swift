
struct CGPoint {
  var x: CGFloat
  var y: CGFloat
  init()
  init(x x: CGFloat, y y: CGFloat)
}

extension CGPoint {
  static var zero: CGPoint { get }
  @_transparent init(x x: Int, y y: Int)
  @_transparent init(x x: Double, y y: Double)
}

extension CGPoint : CustomReflectable, CustomPlaygroundQuickLookable {
}

extension CGPoint : CustomDebugStringConvertible {
}

extension CGPoint : Equatable {
}
struct CGSize {
  var width: CGFloat
  var height: CGFloat
  init()
  init(width width: CGFloat, height height: CGFloat)
}

extension CGSize {
  static var zero: CGSize { get }
  @_transparent init(width width: Int, height height: Int)
  @_transparent init(width width: Double, height height: Double)
}

extension CGSize : CustomReflectable, CustomPlaygroundQuickLookable {
}

extension CGSize : CustomDebugStringConvertible {
}

extension CGSize : Equatable {
}
struct CGVector {
  var dx: CGFloat
  var dy: CGFloat
  init()
  init(dx dx: CGFloat, dy dy: CGFloat)
}

extension CGVector {
  static var zero: CGVector { get }
  @_transparent init(dx dx: Int, dy dy: Int)
  @_transparent init(dx dx: Double, dy dy: Double)
}

extension CGVector : Equatable {
}
struct CGRect {
  var origin: CGPoint
  var size: CGSize
  init()
  init(origin origin: CGPoint, size size: CGSize)
}

extension CGRect {
  static var zero: CGRect { get }
  @_transparent init(x x: CGFloat, y y: CGFloat, width width: CGFloat, height height: CGFloat)
  @_transparent init(x x: Double, y y: Double, width width: Double, height height: Double)
  @_transparent init(x x: Int, y y: Int, width width: Int, height height: Int)
  @_transparent mutating func standardizeInPlace()
  @_transparent mutating func makeIntegralInPlace()
  @_transparent mutating func insetInPlace(dx dx: CGFloat, dy dy: CGFloat)
  @_transparent mutating func offsetInPlace(dx dx: CGFloat, dy dy: CGFloat)
  @_transparent mutating func unionInPlace(_ rect: CGRect)
  @_transparent mutating func intersectInPlace(_ rect: CGRect)
  @warn_unused_result
  @_transparent func divide(_ atDistance: CGFloat, fromEdge fromEdge: CGRectEdge) -> (slice: CGRect, remainder: CGRect)
}

extension CGRect : CustomReflectable, CustomPlaygroundQuickLookable {
}

extension CGRect : CustomDebugStringConvertible {
}

extension CGRect : Equatable {
}
enum CGRectEdge : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case minXEdge
  case minYEdge
  case maxXEdge
  case maxYEdge
}
extension CGPoint {
  @available(tvOS 2.0, *)
  @discardableResult
  func equalTo(_ point2: CGPoint) -> Bool
  @available(tvOS 2.0, *)
  @discardableResult
  func createDictionaryRepresentation() -> CFDictionary
  @available(tvOS 2.0, *)
  @discardableResult
  mutating func makeWithDictionaryRepresentation(_ dict: CFDictionary?) -> Bool
}
extension CGSize {
  @available(tvOS 2.0, *)
  @discardableResult
  func equalTo(_ size2: CGSize) -> Bool
  @available(tvOS 2.0, *)
  @discardableResult
  func createDictionaryRepresentation() -> CFDictionary
  @available(tvOS 2.0, *)
  @discardableResult
  mutating func makeWithDictionaryRepresentation(_ dict: CFDictionary?) -> Bool
}
extension CGRect {
  @available(tvOS 2.0, *)
  static let null: CGRect
  @available(tvOS 2.0, *)
  static let infinite: CGRect
  @available(tvOS 2.0, *)
  var minX: CGFloat { get }
  @available(tvOS 2.0, *)
  var midX: CGFloat { get }
  @available(tvOS 2.0, *)
  var maxX: CGFloat { get }
  @available(tvOS 2.0, *)
  var minY: CGFloat { get }
  @available(tvOS 2.0, *)
  var midY: CGFloat { get }
  @available(tvOS 2.0, *)
  var maxY: CGFloat { get }
  @available(tvOS 2.0, *)
  var width: CGFloat { get }
  @available(tvOS 2.0, *)
  var height: CGFloat { get }
  @available(tvOS 2.0, *)
  @discardableResult
  func equalTo(_ rect2: CGRect) -> Bool
  @available(tvOS 2.0, *)
  var standardized: CGRect { get }
  @available(tvOS 2.0, *)
  var isEmpty: Bool { get }
  @available(tvOS 2.0, *)
  var isNull: Bool { get }
  @available(tvOS 2.0, *)
  var isInfinite: Bool { get }
  @available(tvOS 2.0, *)
  @discardableResult
  func insetBy(dx dx: CGFloat, dy dy: CGFloat) -> CGRect
  @available(tvOS 2.0, *)
  var integral: CGRect { get }
  @available(tvOS 2.0, *)
  @discardableResult
  func union(_ r2: CGRect) -> CGRect
  @available(tvOS 2.0, *)
  @discardableResult
  func intersect(_ r2: CGRect) -> CGRect
  @available(tvOS 2.0, *)
  @discardableResult
  func offsetBy(dx dx: CGFloat, dy dy: CGFloat) -> CGRect
  @available(tvOS 2.0, *)
  func divide(slice slice: UnsafeMutablePointer<CGRect>, remainder remainder: UnsafeMutablePointer<CGRect>, amount amount: CGFloat, edge edge: CGRectEdge)
  @available(tvOS 2.0, *)
  @discardableResult
  func contains(_ point: CGPoint) -> Bool
  @available(tvOS 2.0, *)
  @discardableResult
  func contains(_ rect2: CGRect) -> Bool
  @available(tvOS 2.0, *)
  @discardableResult
  func intersects(_ rect2: CGRect) -> Bool
  @available(tvOS 2.0, *)
  @discardableResult
  func createDictionaryRepresentation() -> CFDictionary
  @available(tvOS 2.0, *)
  @discardableResult
  mutating func makeWithDictionaryRepresentation(_ dict: CFDictionary?) -> Bool
}
extension CGVector {
}
@discardableResult
func __CGPointEqualToPoint(_ point1: CGPoint, _ point2: CGPoint) -> Bool
@discardableResult
func __CGSizeEqualToSize(_ size1: CGSize, _ size2: CGSize) -> Bool
