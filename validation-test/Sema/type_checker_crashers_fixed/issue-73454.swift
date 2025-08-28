// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/73454

func object_method_bind_ptrcall_v(
  _: UnsafeMutableRawPointer?...
) {
}

func setDensity(_ density: Double, ptr: UnsafeMutableRawPointer) {
  var copy_density = density
  var arr: [Int] = []

  object_method_bind_ptrcall_v(&copy_density)
  object_method_bind_ptrcall_v(&arr)

  object_method_bind_ptrcall_v(&arr, &copy_density)

  object_method_bind_ptrcall_v(ptr, &copy_density, &arr)
  object_method_bind_ptrcall_v(&copy_density, ptr, &arr)
}
