// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/FakeFoundation.swiftmodule -module-name FakeFoundation %S/Inputs/FakeFoundation.swift -import-objc-header %S/Inputs/FakeFoundation.h -disable-availability-checking

// REQUIRES: objc_interop

import FakeFoundation

// CHECK: declref_expr type='module<SomeModule>'
// CHECK-NEXT: type_expr type='Data.Type'
let type = SomeModule.Data.self
