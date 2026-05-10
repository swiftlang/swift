// RUN: %target-swift-frontend -module-name SomeModule -typecheck -verify -import-objc-header %S/Inputs/imported_type.h %s -verify-ignore-unrelated

// REQUIRES: objc_interop

import Foundation

let type = SomeModule.Data.self // expected-error {{ambiguous use of 'Data'}}
