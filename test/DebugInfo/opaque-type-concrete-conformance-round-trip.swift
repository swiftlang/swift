// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/A.swift -module-name A \
// RUN:   -emit-module-path %t/A.swiftmodule
// RUN: %target-swift-frontend -emit-ir %t/main.swift -I %t -module-name main \
// RUN:   -parse-as-library -g -Onone -enable-round-trip-debug-types -o - \
// RUN:   | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir %t/main.swift -I %t -module-name main \
// RUN:   -parse-as-library -g -O -enable-round-trip-debug-types -o - \
// RUN:   | %FileCheck %s

// The substitution map of `x`'s type stores the concrete conformance
// `Int: Hashable` for `U.W.A: Hashable` (rdar://180550798).

// CHECK: !DILocalVariable(name: "x"

//--- A.swift
public protocol P { associatedtype A }
public protocol Q { associatedtype W: P }
public struct S<T: P>: P where T.A: Hashable {
  public typealias A = T.A
  public init() {}
}
struct Box<X: P> { var x: X }
public func f<X: P>(_ x: X) -> some Any { Box(x: x) }

//--- main.swift
import A

func h<U: Q>(_ u: U) -> some P where U.W.A: Hashable { S<U.W>() }

@inline(never)
public func g<U: Q>(_ u: U) where U.W.A == Int {
  let x = f(h(u))
  print(x)
}
