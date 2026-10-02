// RUN: %target-run-simple-swift(-Xfrontend -disable-availability-checking) | %FileCheck %s

// REQUIRES: executable_test

// UNSUPPORTED: back_deployment_runtime || use_os_stdlib

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

struct Inner: ~Copyable {
  var x = 0
  var a = UniqueArray<Int>()
}

struct Outer: ~Copyable {
  var types = UniqueArray<Inner>()
}

func test() {
  let p = UnsafeMutablePointer<Outer>.allocate(capacity: 1)
  p.initialize(to: Outer())
  p.pointee.types.append(Inner())
  p.pointee.types[0].a.append(7)

  p[].types[0].x += 1

  // CHECK: a[0] = 7
  print("a[0] = \(p.pointee.types[0].a[0])")
  // CHECK-NEXT: x = 1
  print("x = \(p.pointee.types[0].x)")

  p.deinitialize(count: 1)
  p.deallocate()
  // CHECK-NEXT: done
  print("done")
}

test()
