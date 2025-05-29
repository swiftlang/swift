// RUN: %empty-directory(%t)

// First test: without cross-module-optimization

// RUN: %target-build-swift -O -wmo -disable-cmo -parse-as-library -DMODULE -emit-module -emit-module-path=%t/Module.swiftmodule -module-name=Module %s
// RUN: %target-build-swift -O -wmo -module-name=Main -I%t %s -c -emit-sil | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-NOCMO %s

// Second test: with cross-module-optimization (which is the default)

// RUN: %target-build-swift -O -wmo -Xfrontend -enable-default-cmo -parse-as-library -DMODULE -emit-module -emit-module-path=%t/Module.swiftmodule -module-name=Module %s
// RUN: %target-build-swift -O -wmo -module-name=Main -I%t %s -c -emit-sil | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-CMO %s

#if MODULE

public protocol P {
  func foo(x: [Int])
  func bar(x: [Int])
  func baz(x: some RandomAccessCollection<Int>)
}

public struct S: P {
  public init() {}

  @inline(never)
  public func foo(x: some RandomAccessCollection<Int>) { }
  @inline(never)
  public func bar(x: [Int]) { }
  @inline(never)
  public func baz(x: some RandomAccessCollection<Int>) { }
}

#else

import Module

public struct Local: P {
  @inline(never)
  public func foo(x: some RandomAccessCollection<Int>) { }
  @inline(never)
  public func bar(x: [Int]) { }
  @inline(never)
  public func baz(x: some RandomAccessCollection<Int>) { }
}

// Don't devirtualize in this case because it's better to call the non-generic function
// (which can be fully specialized in the module) via the witness table than the generic
// de-virtualized function.

// CHECK-LABEL: sil @$s4Main24testGenericInOtherModuleyyF
// CHECK-NOCMO:   [[F:%[0-9]+]] = witness_method $S, #P.foo
// CHECK-CMO:     [[F:%[0-9]+]] = function_ref @$s6Module1SV3foo1xyx_tSkRzSi7ElementRtzlFSaySiG_Ttg{{.*}}
// CHECK:         apply [[F]]
// CHECK:       } // end sil function '$s4Main24testGenericInOtherModuleyyF'
public func testGenericInOtherModule() {
  let s = S()
  callFoo(s, x: [])
}

// CHECK-LABEL: sil @$s4Main27testNonGenericInOtherModuleyyF
// CHECK:         [[F:%[0-9]+]] = function_ref @$s6Module1SV3bar1xySaySiG_tF
// CHECK:         apply [[F]]
// CHECK:       } // end sil function '$s4Main27testNonGenericInOtherModuleyyF'
public func testNonGenericInOtherModule() {
  let s = S()
  callBar(s, x: [])
}

// CHECK-LABEL: sil @$s4Main35testGenericRequirementInOtherModuleyyF
// CHECK:         [[F:%[0-9]+]] = function_ref @$s6Module1SV3baz1xyx_tSkRzSi7ElementRtzlF{{.*}}
// CHECK:         apply [[F]]
// CHECK:       } // end sil function '$s4Main35testGenericRequirementInOtherModuleyyF'
public func testGenericRequirementInOtherModule() {
  let s = S()
  callBaz(s, x: [])
}

// CHECK-LABEL: sil @$s4Main23testGenericInSameModuleyyF
// CHECK:         [[F:%[0-9]+]] = function_ref @$s4Main5LocalV3foo1xyx_tSkRzSi7ElementRtzlFSaySiG_Ttg5
// CHECK:         apply [[F]]
// CHECK:       } // end sil function '$s4Main23testGenericInSameModuleyyF'
public func testGenericInSameModule() {
  let l = Local()
  callFoo(l, x: [])
}

func callFoo<T: P>(_ f: T, x: [Int]) {
  f.foo(x: x)
}

func callBar<T: P>(_ f: T, x: [Int]) {
  f.bar(x: x)
}

func callBaz<T: P>(_ f: T, x: [Int]) {
  f.baz(x: x)
}

#endif
