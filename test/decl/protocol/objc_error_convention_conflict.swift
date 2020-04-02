// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s -import-objc-header %S/Inputs/objc_error_convention_conflict.h
// REQUIRES: objc_interop

class CallerXYImpl: CallerX, CallerY {
  func use(_ x: Int, error callback: @escaping () -> ()) throws { } // expected-error {{incompatible error argument conventions}}
}

class CallerXDerivedY: CallerBaseY, CallerX {
  override func use(_ x: Int, error callback: @escaping () -> ()) throws { } // expected-error {{incompatible error argument conventions}}
}
