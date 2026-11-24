// RUN: %target-run-simple-swift

// REQUIRES: executable_test
// XFAIL: swift_test_mode_optimize_none_with_opaque_values 

// rdar://188571852
//
// Mutating an element of a UniqueArray, whose subscript uses a `mutate`
// accessor, reached through a `_modify` coroutine used to destroy the element
// right after the mutation.

extension UnsafeMutablePointer where Pointee: ~Copyable {
  subscript() -> Pointee {
    _read { yield pointee }
    nonmutating _modify { yield &pointee }
  }
}

@available(SwiftStdlib 6.4, *)
struct Inner: ~Copyable {
  var x = 0
  var a = UniqueArray<Int>()
}

@available(SwiftStdlib 6.4, *)
struct Outer: ~Copyable {
  var types = UniqueArray<Inner>()
}

@available(SwiftStdlib 6.4, *)
func test() {
  let p = UnsafeMutablePointer<Outer>.allocate(capacity: 1)
  p.initialize(to: Outer())
  p.pointee.types.append(Inner())
  p.pointee.types[0].a.append(7)

  p[].types[0].x += 1

  precondition(p.pointee.types[0].a[0] == 7)
  precondition(p.pointee.types[0].x == 1)

  p.deinitialize(count: 1)
  p.deallocate()
}

if #available(SwiftStdlib 6.4, *) {
  test()
}
