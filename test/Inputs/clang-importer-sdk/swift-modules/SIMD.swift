extension Float {
  @_alignment(16) public struct Vector4 { public var x, y, z, w: Float }
}

extension Double {
  @_alignment(16) public struct Vector2 { public var x, y: Double }
}

extension Int32 {
  @_alignment(16) public struct Vector3 { public var x, y, z: Int32 }
}
