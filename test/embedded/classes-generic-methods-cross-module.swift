// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -wmo -O %t/Lib.swift -module-name Lib -emit-module -emit-module-path %t/Lib.swiftmodule -c -o %t/Lib.o
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -wmo -O -I %t %t/main.swift -module-name main -c -o %t/main.o
// RUN: %target-embedded-link %t/Lib.o %t/main.o -o %t/a.out %target-clang-resource-dir-opt -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// A client cannot override the library's generic method. Because the library
// was forced to declare it non-`open`, ordinary access control already rejects
// this -- which is the point: the embedded rule turns what would have been a
// link-time or call-site problem into an existing, well-understood one.
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -wmo -O -I %t -verify %t/bad.swift -module-name bad -c -o /dev/null

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

//--- Lib.swift

open class Widget {
  public var tag: Int32
  public init(tag: Int32) { self.tag = tag }

  // Statically dispatched, and not `open`, so no client can override it.
  public func measure<T>(_: T) -> Int32 { tag &+ Int32(MemoryLayout<T>.size) }

  // Ordinary virtual method: clients may override this.
  open func describe() -> Int32 { tag }
}

//--- main.swift

import Lib

final class Gadget: Widget {
  override func describe() -> Int32 { tag &+ 1 }
}

@inline(never) func measureIt(_ w: Widget) -> Int32 { w.measure(Int32(0)) }
@inline(never) func describeIt(_ w: Widget) -> Int32 { w.describe() }

@main
struct Main {
  static func main() {
    let w = Widget(tag: 10)
    let g = Gadget(tag: 20)

    // The generic method is the library's implementation in both cases.
    print(measureIt(w) == 14 ? "OK!" : "FAIL")   // CHECK: OK!
    print(measureIt(g) == 24 ? "OK!" : "FAIL")   // CHECK-NEXT: OK!

    // The non-generic `open` method still dispatches virtually.
    print(describeIt(w) == 10 ? "OK!" : "FAIL")  // CHECK-NEXT: OK!
    print(describeIt(g) == 21 ? "OK!" : "FAIL")  // CHECK-NEXT: OK!
  }
}

//--- bad.swift

import Lib

class Broken: Widget {
  override func measure<T>(_: T) -> Int32 { 0 }
  // expected-error@-1{{overriding non-open instance method outside of its defining module}}
  // expected-error@-2{{generic instance method 'measure' in a class cannot override another method in Embedded Swift}}
}
