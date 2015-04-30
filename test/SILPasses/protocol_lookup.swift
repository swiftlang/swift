// RUN: %target-swift-frontend -O -emit-sil -primary-file %s | FileCheck %s

// Check that this file can be compiled using -O at all (it used to crash due to a bug in the SILCloner)
// Check that it can be compiled correctly.

// Protocol lookup for metatypes.
protocol StaticFoo {
  static func foo() -> String
}

class StaticBar {
  // Check that the cast is not folded as a failing cast.
  // CHECK-LABEL: sil hidden [noinline] @_TZFC15protocol_lookup9StaticBar12mightHaveFoofMS0_FT_SS
  // Check that the cast was not eliminated.
  // CHECK: checked_cast_addr_br
  @inline(never)
  class func mightHaveFoo() -> String {
    if let selfAsFoo = self as? StaticFoo.Type {
      return selfAsFoo.foo()
    } else {
      return "no Foo for you"
    }
  }
}

class StaticWibble : StaticBar, StaticFoo {
  static func foo() -> String { return "StaticWibble.foo" }
}

// CHECK: StaticWibble.foo
println(StaticWibble.mightHaveFoo())

