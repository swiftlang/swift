// Deinit bodies that touch `self` without letting it escape, which must not
// trap. The companion to deinit-self-escape-traps.swift.

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -module-name test %s -c -o %t/a.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/a.o -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// RUN: %target-swift-frontend -O -enable-experimental-feature Embedded -parse-as-library -module-name test %s -c -o %t/a.O.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/a.O.o -o %t/a.O.out -dead_strip
// RUN: %target-run %t/a.O.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded
// REQUIRES: PTRSIZE=64

var sink: Int = 0

// A strong reference to `self` taken and dropped within the deinit.
final class LocalStrong {
  let id: Int
  init(id: Int) { self.id = id }
  deinit {
    let local = self
    sink = local.id
    print("localStrong \(sink)")
    // CHECK: localStrong 1
  }
}

// `self` passed to a function that does not keep it.
@inline(never) func readID(_ o: Passed) -> Int { return o.id }

final class Passed {
  let id: Int
  init(id: Int) { self.id = id }
  deinit {
    print("passed \(readID(self))")
    // CHECK: passed 2
  }
}

// A closure over `self` that does not outlive the deinit.
final class ScopedClosure {
  let id: Int
  init(id: Int) { self.id = id }
  deinit {
    let f = { self.id }
    print("scopedClosure \(f())")
    // CHECK: scopedClosure 3
  }
}

// A weak reference to `self` formed during deinit, which reads as nil.
final class WeakHolder {
  weak var ref: WeakSelf?
}

final class WeakSelf {
  let id: Int
  let holder = WeakHolder()
  init(id: Int) { self.id = id }
  deinit {
    holder.ref = self
    print("weakSelf \(holder.ref == nil ? -1 : holder.ref!.id)")
    // CHECK: weakSelf -1
  }
}

// An unowned(unsafe) reference to `self`, which performs no reference counting.
final class UnsafeHolder {
  unowned(unsafe) var ref: UnsafeSelf?
}

final class UnsafeSelf {
  let id: Int
  let holder = UnsafeHolder()
  init(id: Int) { self.id = id }
  deinit {
    holder.ref = self
    print("unsafeSelf \(holder.ref!.id)")
    // CHECK: unsafeSelf 5
  }
}

@inline(never) func run() {
  var a: LocalStrong? = LocalStrong(id: 1)
  a = nil
  var b: Passed? = Passed(id: 2)
  b = nil
  var c: ScopedClosure? = ScopedClosure(id: 3)
  c = nil
  var d: WeakSelf? = WeakSelf(id: 4)
  d = nil
  var e: UnsafeSelf? = UnsafeSelf(id: 5)
  e = nil
}

@main
struct Main {
  static func main() {
    run()
    print("end")
    // CHECK: end
  }
}
