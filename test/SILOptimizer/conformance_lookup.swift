// RUN: %empty-directory(%t) 
// RUN: split-file %s %t

// RUN: %target-build-swift -O -wmo -parse-as-library -emit-module -emit-module-path=%t/Module.swiftmodule -module-name=Module %t/module.swift -c -o %t/module.o
// RUN: %target-build-swift -O -wmo -module-name=Main -I %t %t/main.swift -c -o %t/main.o
// RUN: %target-swiftc_driver %t/main.o %t/module.o -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s -check-prefix=CHECK-OUTPUT

// RUN: %target-build-swift -O -wmo -parse-as-library %t/module.swift -module-name=Module -emit-sil | %FileCheck %s  -check-prefix=CHECK-SIL -check-prefix=CHECK-SIL-MODULE
// RUN: %target-build-swift -O -wmo -module-name=Main -I %t %t/main.swift -emit-sil | %FileCheck %s -check-prefix=CHECK-SIL

// RUN: %target-build-swift -O -wmo -parse-as-library %t/module.swift -module-name=Module -emit-ir | %FileCheck %s  -check-prefix=CHECK-IR-MODULE -check-prefix=CHECK-IR-MODULE-%target-ptrsize
// RUN: %target-build-swift -O -wmo -module-name=Main -I %t %t/main.swift -emit-ir | %FileCheck %s -check-prefix=CHECK-IR-MAIN -check-prefix=CHECK-IR-MAIN-%target-ptrsize

// REQUIRES: executable_test

// Windows doesn't use fixed class metadata layout
// UNSUPPORTED: OS=windows-msvc

//--- module.swift

@_semantics("fast_cast")
public protocol P: C {
  func foo()
}

@_semantics("fast_cast")
public protocol P2: P {
  func foo2()
}

@_semantics("fast_cast")
public protocol Q: F {
  func bar()
}

public class Base {
  public init() {}
}

// CHECK-SIL-LABEL: sil_vtable C {
// CHECK-SIL-NEXT:    no_conformance P
// CHECK-SIL-NEXT:    no_conformance P2
// CHECK-SIL-NEXT:    #Base.init
// CHECK-IR-MODULE-LABEL: @"$s6Module1CCMf" = internal global {{.*}} <{ ptr null, ptr null, ptr null, ptr @"$s6Module1CCfD{{.*}}",
public class C: Base {
  override public init() { super.init() }
}

// CHECK-SIL-LABEL: sil_vtable D {
// CHECK-SIL-NEXT:    conformance D: P module Module
// CHECK-SIL-NEXT:    no_conformance P2
// CHECK-SIL-NEXT:    #Base.init
// CHECK-IR-MODULE-LABEL: @"$s6Module1DCMf" = internal global {{.*}} <{ ptr null, ptr @"$s6Module1DCAA1PAAWP", ptr null, ptr @"$s6Module1DCfD{{.*}}",
public final class D: C, P {
  var x = 17

  override public init() { super.init() }

  public func foo() {
    print(x)
  }
}

// CHECK-SIL-LABEL: sil_vtable D2 {
// CHECK-SIL-NEXT:    conformance D2: P module Module
// CHECK-SIL-NEXT:    conformance D2: P2 module Module
// CHECK-SIL-NEXT:    #Base.init
// CHECK-IR-MODULE-LABEL: @"$s6Module2D2CMf" = internal global {{.*}} <{ ptr @"$s6Module2D2CAA2P2AAWP", ptr @"$s6Module2D2CAA1PAAWP", ptr null, ptr @"$s6Module2D2CfD{{.*}}",
public final class D2: C, P2 {
  var x = 27

  override public init() { super.init() }

  public func foo() {
    print(x)
  }
  public func foo2() {
    print("D2.foo2")
  }
}

// CHECK-SIL-LABEL: sil_vtable E {
// CHECK-SIL-NEXT:    no_conformance P
// CHECK-SIL-NEXT:    no_conformance P2
// CHECK-SIL-NEXT:    #Base.init
// CHECK-IR-MODULE-LABEL: @"$s6Module1ECMf" = internal global {{.*}} <{ ptr null, ptr null, ptr null, ptr @"$s6Module1ECfD{{.*}}",
public class E: C {
  override public init() {}
}

// CHECK-SIL-MODULE-LABEL: sil_vtable E2 {
// CHECK-SIL-MODULE-NEXT:    conformance E2: P module Module
// CHECK-SIL-MODULE-NEXT:    no_conformance P2
// CHECK-SIL-MODULE-NEXT:    #Base.init
// CHECK-IR-MODULE-LABEL: @"$s6Module2E2CMf" = internal global {{.*}} <{ ptr null, ptr @"$s6Module2E2CAA1PAAWP", ptr null, ptr @"$s6Module2E2CfD{{.*}}",
public class E2: E, P {
  public func foo() {
    print("E2.foo")
  }
}
// CHECK-SIL-LABEL: sil_vtable F {
// CHECK-SIL-NEXT:    no_conformance P
// CHECK-SIL-NEXT:    no_conformance P2
// CHECK-SIL-NEXT:    no_conformance Q
// CHECK-SIL-NEXT:    #Base.init
// CHECK-IR-MODULE-LABEL: @"$s6Module1FCMf" = internal global {{.*}} <{ ptr null, ptr null, ptr null, ptr null, ptr @"$s6Module1FCfD{{.*}}",
public class F: E {
  override public init() {}
}

// CHECK-SIL-LABEL: sil_vtable G {
// CHECK-SIL-NEXT:    no_conformance P
// CHECK-SIL-NEXT:    no_conformance P2
// CHECK-SIL-NEXT:    conformance G: Q module Module
// CHECK-SIL-NEXT:    #Base.init
// CHECK-IR-MODULE-LABEL: @"$s6Module1GCMf" = internal global {{.*}} <{ ptr @"$s6Module1GCAA1QAAWP", ptr null, ptr null, ptr null, ptr @"$s6Module1GCfD{{.*}}",
public class G: F, Q {
  override public init() {}

  public func bar() {
    print("G.bar")
  }
}

// CHECK-SIL-LABEL: sil_vtable H {
// CHECK-SIL-NEXT:    no_conformance P
// CHECK-SIL-NEXT:    no_conformance P2
// CHECK-SIL-NEXT:    conformance H: inherit (G: Q module Module)
// CHECK-SIL-NEXT:    #Base.init
// CHECK-IR-MODULE-LABEL: @"$s6Module1HCMf" = internal global {{.*}} <{ ptr @"$s6Module1GCAA1QAAWP", ptr null, ptr null, ptr null, ptr @"$s6Module1HCfD{{.*}}",
public final class H: G {
  override public init() {}
}

// CHECK-SIL-LABEL: sil_vtable I {
// CHECK-SIL-NEXT:    no_conformance P
// CHECK-SIL-NEXT:    no_conformance P2
// CHECK-SIL-NEXT:    #Base.init
// CHECK-IR-MODULE-LABEL: @"$s6Module1ICMf" = internal global {{.*}} <{ ptr null, ptr null, ptr null, ptr @"$s6Module1ICfD{{.*}}",
public final class I: E {
  override public init() {}
}

@_semantics("fast_cast")
public protocol P3: C3 {
  func foo()
}

// CHECK-SIL-MODULE-LABEL: sil_vtable C3 {
// CHECK-SIL-MODULE-NEXT:    #C3.deinit!deallocator:
public class C3 {
}

// CHECK-SIL-MODULE-LABEL: sil_vtable D3 {
// CHECK-SIL-MODULE-NEXT:    #D3.deinit!deallocator:
public final class D3<T>: C3, P3 {
  public func foo() {}
}

@_semantics("fast_cast")
public protocol P4: C4 {
  func foo()
}

// CHECK-SIL-MODULE-LABEL: sil_vtable C4 {
// CHECK-SIL-MODULE-NEXT:    #C4.deinit!deallocator:
open class C4 {
}

// CHECK-SIL-MODULE-LABEL: sil_vtable D4 {
// CHECK-SIL-MODULE-NEXT:    #D4.foo
open class D4: C4, P4 {
  public func foo() {}
}

// CHECK-IR-MODULE-LABEL: define {{.*}} @"$s6Module6cast2PyAA1P_pSgAA1CCF"
// CHECK-IR-MODULE:         %1 = load ptr, ptr %0  
// CHECK-IR-MODULE-64:      [[G:%.*]] = getelementptr i8, ptr %{{[0-9]+}}, i64 -32
// CHECK-IR-MODULE-32:      [[G:%.*]] = getelementptr i8, ptr %{{[0-9]+}}, i32 -16
// CHECK-IR-MODULE-NEXT:    [[L:%.*]] = load ptr, ptr [[G]]
// CHECK-IR-MODULE-NEXT:    icmp eq ptr [[L]], null
// CHECK:                   ret
@inline(never)
func cast2P(_ c: C) -> P? {
  return c as? P
}

// CHECK-IR-MODULE-LABEL: define {{.*}} @"$s6Module7cast2P2yAA0C0_pSgAA1CCF"
// CHECK-IR-MODULE:         %1 = load ptr, ptr %0  
// CHECK-IR-MODULE-64:      [[G:%.*]] = getelementptr i8, ptr %{{[0-9]+}}, i64 -40
// CHECK-IR-MODULE-32:      [[G:%.*]] = getelementptr i8, ptr %{{[0-9]+}}, i32 -20
// CHECK-IR-MODULE-NEXT:    [[L:%.*]] = load ptr, ptr [[G]]
// CHECK-IR-MODULE-NEXT:    icmp eq ptr [[L]], null
// CHECK:                   ret
@inline(never)
func cast2P2(_ c: C) -> P2? {
  return c as? P2
}

// CHECK-IR-MODULE-LABEL: define {{.*}} @"$s6Module6cast2QyAA1Q_pSgAA1FCF"
// CHECK-IR-MODULE:         %1 = load ptr, ptr %0  
// CHECK-IR-MODULE-64:      [[G:%.*]] = getelementptr i8, ptr %{{[0-9]+}}, i64 -48
// CHECK-IR-MODULE-32:      [[G:%.*]] = getelementptr i8, ptr %{{[0-9]+}}, i32 -24
// CHECK-IR-MODULE-NEXT:    [[L:%.*]] = load ptr, ptr [[G]]
// CHECK-IR-MODULE-NEXT:    icmp eq ptr [[L]], null
// CHECK:                   ret
@inline(never)
func cast2Q(_ c: F) -> Q? {
  return c as? Q
}

@inline(never)
func testC2P(_ c: C) {
  if let p = cast2P(c) {
    p.foo()
  } else {
    print("nil")
  }
}

@inline(never)
func testC2P2(_ c: C) {
  if let p = cast2P2(c) {
    p.foo2()
  } else {
    print("nil")
  }
}

@inline(never)
func testF2Q(_ c: F) {
  if let q = cast2Q(c) {
    q.bar()
  } else {
    print("nil")
  }
}

@inline(never)
func testCombined(_ c: C) {
  if let q = c as? (E & P) {
    q.foo()
  } else {
    print("nil")
  }
}

@inline(never)
public func testPInModule() {
  // CHECK-OUTPUT-LABEL: ### testPInModule
  print("### testPInModule")
  // CHECK-OUTPUT-NEXT: nil
  testC2P(C())
  // CHECK-OUTPUT-NEXT: 17
  testC2P(D())
  // CHECK-OUTPUT-NEXT: 27
  testC2P(D2())
  // CHECK-OUTPUT-NEXT: nil
  testC2P(E())
  // CHECK-OUTPUT-NEXT: nil
  testC2P(F())
  // CHECK-OUTPUT-NEXT: nil
  testC2P(G())
  // CHECK-OUTPUT-NEXT: nil
  testC2P(H())
  // CHECK-OUTPUT-NEXT: nil
  testC2P(I())
}

@inline(never)
public func testP2InModule() {
  // CHECK-OUTPUT-LABEL: ### testP2InModule
  print("### testP2InModule")
  // CHECK-OUTPUT-NEXT: nil
  testC2P2(C())
  // CHECK-OUTPUT-NEXT: nil
  testC2P2(D())
  // CHECK-OUTPUT-NEXT: D2.foo2
  testC2P2(D2())
  // CHECK-OUTPUT-NEXT: nil
  testC2P2(E())
  // CHECK-OUTPUT-NEXT: nil
  testC2P2(F())
  // CHECK-OUTPUT-NEXT: nil
  testCombined(D())
  // CHECK-OUTPUT-NEXT: nil
  testCombined(E())
  // CHECK-OUTPUT-NEXT: E2.foo
  testCombined(E2())
}

@inline(never)
public func testQInModule() {
  // CHECK-OUTPUT-LABEL: ### testQInModule
  print("### testQInModule")
  // CHECK-OUTPUT-NEXT: nil
  testF2Q(F())
  // CHECK-OUTPUT-NEXT: G.bar
  testF2Q(G())
  // CHECK-OUTPUT-NEXT: G.bar
  testF2Q(H())
}

//--- main.swift

import Module

// CHECK-IR-MAIN-LABEL: define {{.*}} @"$s4Main6cast2Py6Module1P_pSgAC1CCF"
// CHECK-IR-MAIN:         %1 = load ptr, ptr %0  
// CHECK-IR-MAIN-64:      [[G:%.*]] = getelementptr i8, ptr %{{[0-9]+}}, i64 -32
// CHECK-IR-MAIN-32:      [[G:%.*]] = getelementptr i8, ptr %{{[0-9]+}}, i32 -16
// CHECK-IR-MAIN-NEXT:    [[L:%.*]] = load ptr, ptr [[G]]
// CHECK-IR-MAIN-NEXT:    icmp eq ptr [[L]], null
// CHECK:                   ret
@inline(never)
func cast2P(_ c: C) -> P? {
  return c as? P
}

// CHECK-IR-MAIN-LABEL: define {{.*}} @"$s4Main7cast2P2y6Module0C0_pSgAC1CCF"
// CHECK-IR-MAIN:         %1 = load ptr, ptr %0  
// CHECK-IR-MAIN-64:      [[G:%.*]] = getelementptr i8, ptr %{{[0-9]+}}, i64 -40
// CHECK-IR-MAIN-32:      [[G:%.*]] = getelementptr i8, ptr %{{[0-9]+}}, i32 -20
// CHECK-IR-MAIN-NEXT:    [[L:%.*]] = load ptr, ptr [[G]]
// CHECK-IR-MAIN-NEXT:    icmp eq ptr [[L]], null
// CHECK:                   ret
@inline(never)
func cast2P2(_ c: C) -> P2? {
  return c as? P2
}

// CHECK-IR-MAIN-LABEL: define {{.*}} @"$s4Main6cast2Qy6Module1Q_pSgAC1FCF"
// CHECK-IR-MAIN:         %1 = load ptr, ptr %0  
// CHECK-IR-MAIN-64:      [[G:%.*]] = getelementptr i8, ptr %{{[0-9]+}}, i64 -48
// CHECK-IR-MAIN-32:      [[G:%.*]] = getelementptr i8, ptr %{{[0-9]+}}, i32 -24
// CHECK-IR-MAIN-NEXT:    [[L:%.*]] = load ptr, ptr [[G]]
// CHECK-IR-MAIN-NEXT:    icmp eq ptr [[L]], null
// CHECK:                   ret
@inline(never)
func cast2Q(_ c: F) -> Q? {
  return c as? Q
}

@inline(never)
func testC2P(_ c: C) {
  if let p = cast2P(c) {
    p.foo()
  } else {
    print("nil")
  }
}

@inline(never)
func testC2P2(_ c: C) {
  if let p = cast2P2(c) {
    p.foo2()
  } else {
    print("nil")
  }
}

@inline(never)
func testF2Q(_ c: F) {
  if let q = cast2Q(c) {
    q.bar()
  } else {
    print("nil")
  }
}

@inline(never)
public func testPInMain() {
  // CHECK-OUTPUT-LABEL: ### testPInMain
  print("### testPInMain")
  // CHECK-OUTPUT-NEXT: nil
  testC2P(C())
  // CHECK-OUTPUT-NEXT: 17
  testC2P(D())
  // CHECK-OUTPUT-NEXT: 27
  testC2P(D2())
  // CHECK-OUTPUT-NEXT: nil
  testC2P(E())
  // CHECK-OUTPUT-NEXT: nil
  testC2P(F())
  // CHECK-OUTPUT-NEXT: nil
  testC2P(G())
  // CHECK-OUTPUT-NEXT: nil
  testC2P(H())
  // CHECK-OUTPUT-NEXT: nil
  testC2P(I())
}

@inline(never)
public func testP2InMain() {
  // CHECK-OUTPUT-LABEL: ### testP2InMain
  print("### testP2InMain")
  // CHECK-OUTPUT-NEXT: nil
  testC2P2(C())
  // CHECK-OUTPUT-NEXT: nil
  testC2P2(D())
  // CHECK-OUTPUT-NEXT: D2.foo2
  testC2P2(D2())
  // CHECK-OUTPUT-NEXT: nil
  testC2P2(E())
  // CHECK-OUTPUT-NEXT: nil
  testC2P2(F())
}

@inline(never)
public func testQInMain() {
  // CHECK-OUTPUT-LABEL: ### testQInModule
  print("### testQInModule")
  // CHECK-OUTPUT-NEXT: nil
  testF2Q(F())
  // CHECK-OUTPUT-NEXT: G.bar
  testF2Q(G())
  // CHECK-OUTPUT-NEXT: G.bar
  testF2Q(H())
}

testPInModule()
testP2InModule()
testQInModule()

testPInMain()
testP2InMain()
testQInMain()

