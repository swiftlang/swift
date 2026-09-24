// Weak and unowned references to immortal objects. An object promoted to static
// storage has refcount immortalRefCount | doNotFreeBit, which the weak/unowned
// entry points must treat as permanently live: loads always succeed, weak and
// unowned counts are not tracked, and the object is never deinited or freed.
//
// The immortal path (swift_initStaticObject) is reached only when the optimizer
// can constant-fold the global's initializer, so it is exercised by the -O and
// -Osize runs. The unoptimized run covers the same source against an ordinary
// heap object.

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -module-name test %s -c -o %t/a.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/a.o -o %t/a.out -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// RUN: %target-swift-frontend -O -enable-experimental-feature Embedded -parse-as-library -module-name test %s -c -o %t/a.O.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/a.O.o -o %t/a.O.out -dead_strip
// RUN: %target-run %t/a.O.out | %FileCheck %s

// RUN: %target-swift-frontend -Osize -enable-experimental-feature Embedded -parse-as-library -module-name test %s -c -o %t/a.Osize.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/a.Osize.o -o %t/a.Osize.out -dead_strip
// RUN: %target-run %t/a.Osize.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: PTRSIZE=64
// REQUIRES: swift_feature_Embedded

final class Immortal {
  let id: Int
  init(id: Int) { self.id = id }
  // An immortal object must never be deinited. Reaching this is a failure, and
  // the CHECK-NOT below catches it.
  deinit { print("UNEXPECTED deinit \(id)") }
}

let global = Immortal(id: 100)

final class WeakHolder {
  weak var ref: Immortal?
  init(_ r: Immortal?) { self.ref = r }
}

final class UnownedHolder {
  unowned var ref: Immortal
  init(_ r: Immortal) { self.ref = r }
}

struct WeakBox {
  weak var ref: Immortal?
}

func load(_ i: Immortal?) -> Int { return i?.id ?? -1 }

// A weak reference to an immortal object always loads.
@inline(never) func weakToImmortal() {
  let h = WeakHolder(global)
  print("weak \(load(h.ref))")
  // CHECK: weak 100
  h.ref = nil
  print("cleared \(load(h.ref))")
  // CHECK: cleared -1
  h.ref = global
  print("restored \(load(h.ref))")
  // CHECK: restored 100
}

// Weak references to an immortal object come and go without affecting it.
@inline(never) func weakScopesToImmortal() {
  for _ in 0 ..< 3 {
    let h = WeakHolder(global)
    print("loop \(load(h.ref))")
    // CHECK: loop 100
    // CHECK: loop 100
    // CHECK: loop 100
  }
  print("stillHere \(global.id)")
  // CHECK: stillHere 100
}

// Copying an address-only box holding a weak reference to an immortal object.
@inline(never) func weakBoxToImmortal() {
  let original = WeakBox(ref: global)
  let copy = original
  print("box \(load(original.ref)) \(load(copy.ref))")
  // CHECK: box 100 100
  let p = UnsafeMutablePointer<WeakBox>.allocate(capacity: 1)
  p.initialize(to: original)
  print("pointer \(load(p.pointee.ref))")
  // CHECK: pointer 100
  p.deinitialize(count: 1)
  p.deallocate()
}

// An unowned reference to an immortal object is always readable.
@inline(never) func unownedToImmortal() {
  let h = UnownedHolder(global)
  print("unowned \(h.ref.id)")
  // CHECK: unowned 100
  h.ref = global
  print("reassigned \(h.ref.id)")
  // CHECK: reassigned 100
  do {
    let h2 = UnownedHolder(global)
    print("scoped \(h2.ref.id)")
    // CHECK: scoped 100
  }
  print("afterScope \(h.ref.id)")
  // CHECK: afterScope 100
}

// A weak slot pointing at an immortal object outliving many strong scopes.
@inline(never) func mixed() {
  let w = WeakHolder(global)
  let u = UnownedHolder(global)
  var strong: Immortal? = global
  print("all \(load(w.ref)) \(u.ref.id) \(load(strong))")
  // CHECK: all 100 100 100
  strong = nil
  print("strongGone \(load(w.ref)) \(u.ref.id)")
  // CHECK: strongGone 100 100
}

@main
struct Main {
  static func main() {
    weakToImmortal()
    weakScopesToImmortal()
    weakBoxToImmortal()
    unownedToImmortal()
    mixed()
    print("end")
    // CHECK: end
    // CHECK-NOT: UNEXPECTED
  }
}
