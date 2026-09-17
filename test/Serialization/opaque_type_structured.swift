// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -target %target-swift-5.1-abi-triple -o %t -enable-library-evolution %S/Inputs/OpaqueTypeStructured.swift
// RUN: %target-swift-frontend -emit-sil -target %target-swift-5.1-abi-triple -I %t %s | %FileCheck -check-prefix=SIL %s
// RUN: %target-swift-ide-test -print-module -source-filename %s -module-to-print OpaqueTypeStructured -I %t | %FileCheck -check-prefix=AST %s
import OpaqueTypeStructured

func acceptR<T: R>(_: T) { }

// AST: func f() -> (some P, [some Q])

func passAnX(x: X) {
  // SIL: $@convention(method) (@in_guaranteed X) -> (@out @_opaqueReturnTypeOf("$s20OpaqueTypeStructured1XV1fQr_SayQR_GtyF", 0) __, @owned Array<@_opaqueReturnTypeOf("$s20OpaqueTypeStructured1XV1fQr_SayQR_GtyF", 1) __>)
  acceptR(x)
  let _: X.A = x.f()
}

