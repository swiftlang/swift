// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -verify -verify-ignore-unrelated -I %S/Inputs/custom-modules -cxx-interoperability-mode=default %s

// REQUIRES: objc_interop

import Foundation
import SwiftErrorCxxFunctions

// A C++ declaration keeps its error parameter. The throwing variant builds its
// parameters from the full Clang parameter list at module scope, which accounts
// for neither 'self', generic parameters, nor an enclosing context.

func useGlobal() throws {
  try sec_cxx_global(1) // expected-error {{missing argument for parameter #2 in call}}
}

func useNamespaceMember() throws {
  try SECNS.sec_in_namespace(1) // expected-error {{missing argument for parameter #2 in call}}
}

func useTemplate(_ p: UnsafeMutablePointer<Int32>) throws {
  try sec_template(p) // expected-error {{missing argument for parameter #2 in call}}
}

func useMethod(_ s: inout SECStruct) throws {
  try s.sec_method(1) // expected-error {{missing argument for parameter #2 in call}}
}

// A declaration with C language linkage still gets the throwing variant.
func useExternC() throws {
  let _: () = try sec_extern_c(1)
}
