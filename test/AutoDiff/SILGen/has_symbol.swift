// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/Library.swiftmodule -parse-as-library %t/Library.swift -enable-library-evolution
// RUN: %target-swift-frontend -emit-silgen %t/Client.swift -I %t -module-name test | %FileCheck %t/Client.swift

// UNSUPPORTED: OS=windows-msvc

//--- Library.swift

import _Differentiation

@differentiable(reverse)
public func foo(_ x: Float) -> Float { x }

@derivative(of: foo)
public func bar(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
  fatalError()
}

//--- Client.swift

@_weakLinked import Library

// CHECK: sil hidden [ossa] @$s4test0A15GlobalFunctionsyyF : $@convention(thin) () -> ()
func testGlobalFunctions() {
  // CHECK: [[RES:%[0-9]+]] = has_symbol #foo
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(foo(_:)) {}

  // CHECK: [[RES:%[0-9]+]] = has_symbol #bar
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(bar(_:)) {}
}

// --- foo(_:) ---
// CHECK: sil @$s7Library3fooyS2fF : $@convention(thin) (Float) -> Float
// CHECK: sil @$s7Library3fooyS2fFTJfSpSr : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float)
// CHECK: sil @$s7Library3fooyS2fFTJrSpSr : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float)
// FIXME: missing reverse-mode differentiability witness for foo(_:)

// --- bar(_:) ---
// CHECK: sil @$s7Library3barySf5value_S2fc8pullbacktSfF : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float)
// FIXME: missing reverse-mode differentiability witness for foo(_:)
