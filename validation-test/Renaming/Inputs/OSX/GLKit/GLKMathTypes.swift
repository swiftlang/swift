
var GLK_SSE3_INTRINSICS: Int32 { get }
struct _GLKMatrix2 {
  var m2: ((Float, Float), (Float, Float))
  var m: (Float, Float, Float, Float)
  init(m2 m2: ((Float, Float), (Float, Float)))
  init(m m: (Float, Float, Float, Float))
  init()
}

extension GLKMatrix2 {
  typealias _Tuple = (Float, Float, Float, Float)
  var _tuple: _Tuple { get }
  var m00: Float { get }
  var m01: Float { get }
  var m10: Float { get }
  var m11: Float { get }
  subscript(_ i: Int) -> Float { get }
}
typealias GLKMatrix2 = _GLKMatrix2
struct _GLKMatrix3 {
  var m: (Float, Float, Float, Float, Float, Float, Float, Float, Float)
  init(m m: (Float, Float, Float, Float, Float, Float, Float, Float, Float))
  init()
}

extension GLKMatrix3 {
  typealias _Tuple = (Float, Float, Float, Float, Float, Float, Float, Float, Float)
  var _tuple: _Tuple { get }
  var m00: Float { get }
  var m01: Float { get }
  var m02: Float { get }
  var m10: Float { get }
  var m11: Float { get }
  var m12: Float { get }
  var m20: Float { get }
  var m21: Float { get }
  var m22: Float { get }
  subscript(_ i: Int) -> Float { get }
}
typealias GLKMatrix3 = _GLKMatrix3
struct _GLKMatrix4 {
  var m: (Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float)
  init(m m: (Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float))
  init()
}

extension GLKMatrix4 {
  typealias _Tuple = (Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float)
  var _tuple: _Tuple { get }
  var m00: Float { get }
  var m01: Float { get }
  var m02: Float { get }
  var m03: Float { get }
  var m10: Float { get }
  var m11: Float { get }
  var m12: Float { get }
  var m13: Float { get }
  var m20: Float { get }
  var m21: Float { get }
  var m22: Float { get }
  var m23: Float { get }
  var m30: Float { get }
  var m31: Float { get }
  var m32: Float { get }
  var m33: Float { get }
  subscript(_ i: Int) -> Float { get }
}
typealias GLKMatrix4 = _GLKMatrix4
struct _GLKVector2 {
  var v: (Float, Float)
  init(v v: (Float, Float))
  init()
}

extension GLKVector2 {
  typealias _Tuple = (Float, Float)
  var _tuple: _Tuple { get }
  var x: Float { get }
  var y: Float { get }
  var s: Float { get }
  var t: Float { get }
  subscript(_ i: Int) -> Float { get }
}
typealias GLKVector2 = _GLKVector2
struct _GLKVector3 {
  var v: (Float, Float, Float)
  init(v v: (Float, Float, Float))
  init()
}

extension GLKVector3 {
  typealias _Tuple = (Float, Float, Float)
  var _tuple: _Tuple { get }
  var x: Float { get }
  var y: Float { get }
  var z: Float { get }
  var s: Float { get }
  var t: Float { get }
  var p: Float { get }
  var r: Float { get }
  var g: Float { get }
  var b: Float { get }
  subscript(_ i: Int) -> Float { get }
}
typealias GLKVector3 = _GLKVector3
struct _GLKVector4 {
  var v: (Float, Float, Float, Float)
  init(v v: (Float, Float, Float, Float))
  init()
}

extension GLKVector4 {
  typealias _Tuple = (Float, Float, Float, Float)
  var _tuple: _Tuple { get }
  var x: Float { get }
  var y: Float { get }
  var z: Float { get }
  var w: Float { get }
  var s: Float { get }
  var t: Float { get }
  var p: Float { get }
  var q: Float { get }
  var r: Float { get }
  var g: Float { get }
  var b: Float { get }
  var a: Float { get }
  subscript(_ i: Int) -> Float { get }
}
typealias GLKVector4 = _GLKVector4
struct _GLKQuaternion {
  var q: (Float, Float, Float, Float)
  init(q q: (Float, Float, Float, Float))
  init()
}

extension GLKQuaternion {
  typealias _Tuple = (Float, Float, Float, Float)
  var _tuple: _Tuple { get }
  var v: GLKVector3 { get }
  var s: Float { get }
  var x: Float { get }
  var y: Float { get }
  var z: Float { get }
  var w: Float { get }
  subscript(_ i: Int) -> Float { get }
}
typealias GLKQuaternion = _GLKQuaternion
