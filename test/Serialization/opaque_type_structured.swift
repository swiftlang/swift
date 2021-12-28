// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t -enable-library-evolution %S/Inputs/OpaqueTypeStructured.swift
// RUN: %target-swift-frontend -emit-sil -I %t %s | %FileCheck -check-prefix=SIL %s
// RUN: %target-swift-ide-test -print-module -source-filename %s -module-to-print OpaqueTypeStructured -I %t | %FileCheck -check-prefix=AST %s
import OpaqueTypeStructured

func acceptR<T: R>(_: T) { }

// AST: func f() -> (some P, [some Q])

func passAnX(x: X) {
  // SIL: $@convention(method) (@in_guaranteed X) -> (@out @_opaqueReturnTypeOf("$s20OpaqueTypeStructured1XV1fQr_SayQrGtyF", 0) __, @owned Array<@_opaqueReturnTypeOf("$s20OpaqueTypeStructured1XV1fQr_SayQrGtyF", 1) __>)
  acceptR(x)
  let _: X.A = x.f()
}

