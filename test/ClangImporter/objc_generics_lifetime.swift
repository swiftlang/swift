// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -I %S/Inputs/custom-modules -import-objc-header %S/Inputs/objc_generics_lifetime.h %s -verify

// REQUIRES: objc_interop

// expected-no-diagnostics

import Foundation

func makeSub() {
  _ = LifetimeConcreteSub(value: NSObject())
}
