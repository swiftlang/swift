// Make sure we don't crash while printing the standard library.
//
// RUN: %target-swift-ide-test -print-module -module-to-print=Swift -source-filename %s -accessibility-filter-public -skip-private-stdlib-decls -fully-qualified-types-if-ambiguous -synthesize-sugar-on-types > %t.txt
// RUN: FileCheck -check-prefix=CHECK-ARGC %s < %t.txt
// RUN: FileCheck %s < %t.txt
// RUN: FileCheck -check-prefix=CHECK-SUGAR %s < %t.txt
// RUN: FileCheck -check-prefix=CHECK-MUTATING-ATTR %s < %t.txt
// RUN: %target-swift-ide-test -print-module -module-to-print=Swift -source-filename %s -accessibility-filter-public -skip-private-stdlib-decls -fully-qualified-types-if-ambiguous -synthesize-sugar-on-types -print-regular-comments | FileCheck -check-prefix=NO-FIXMES %s
// RUN: %target-swift-ide-test -print-module -module-to-print=Swift -source-filename %s -module-print-submodules | FileCheck -check-prefix=CHECK-ARGC %s

// CHECK-ARGC: static var argc: CInt { get }

// CHECK-NOT: {{^}}import
// CHECK-NOT: _Double
// CHECK-NOT: _StringBuffer
// CHECK-NOT: _StringCore
// CHECK-NOT: _ArrayBody
// CHECK-NOT: func ~>
// FIXME: Builtin.
// FIXME: RawPointer
// CHECK-NOT: extension [
// CHECK-NOT: extension {{.*}}?
// CHECK-NOT: extension {{.*}}!
// CHECK-NOT: addressWithOwner
// CHECK-NOT: mutableAddressWithOwner

// CHECK-SUGAR: extension Array :
// CHECK-SUGAR: extension ImplicitlyUnwrappedOptional :
// CHECK-SUGAR: extension Optional :

// CHECK-MUTATING-ATTR: mutating func

// NO-FIXMES-NOT: FIXME
