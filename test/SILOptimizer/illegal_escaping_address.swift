// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -enforce-exclusivity=unchecked -parse-as-library %s

// Check that the compiler does not crash for illegal escaping of an address
// of a local variable.

var gg = 0

@inline(never)
public func consume(_ x: Int) {
  gg = x
}

public func test_load_after_dealloc_multi_block(b: Bool) {
  var p: UnsafePointer<Int>? = nil
  if (true) {
    var local = 42
    p = withUnsafePointer(to: &local) { p in
      return p
    }
  }
  if b {
    consume(p!.pointee)
  }
}

public func test_load_after_dealloc_single_block() {
  var p: UnsafePointer<Int>? = nil
  if (true) {
    var local = 42
    p = withUnsafePointer(to: &local) { p in
      return p
    }
  }
  consume(p!.pointee)
}

public func test_address_escaping_function_multi_block(b: Bool) -> UnsafePointer<Int>? {
  var local = 42
  let p: UnsafePointer<Int> = withUnsafePointer(to: &local) { p in return p }
  if b {
    return p
  }
  return nil
}

public func test_address_escaping_function_single_block() -> UnsafePointer<Int> {
  var local = 42
  let p: UnsafePointer<Int> = withUnsafePointer(to: &local) { p in return p }
  return p
}

