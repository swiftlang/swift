// RUN: %target-swift-frontend -module-name SomeModule -typecheck -verify -dump-ast -import-objc-header %S/Inputs/imported_type.h %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

// CHECK: declref_expr type="module<SomeModule>"
// CHECK-NEXT: type_expr type="Data.Type"
let type = SomeModule.Data.self
