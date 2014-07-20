// Make sure we don't crash while printing the standard library.
//
// RUN: %swift-ide-test -print-module -module-to-print=Swift -source-filename %s -skip-private-stdlib-decls -fully-qualified-types-if-ambiguous -synthesize-sugar-on-types > %t.txt
// RUN: FileCheck -check-prefix=CHECK-ARGC -input-file %t.txt %s
// RUN: FileCheck -input-file %t.txt %s
// RUN: FileCheck -check-prefix=CHECK-SUGAR -input-file %t.txt %s
// RUN: FileCheck -check-prefix=CHECK-MUTATING-ATTR -input-file %t.txt %s
// RUN: %swift-ide-test -print-module -module-to-print=Swift -source-filename %s -module-print-submodules | FileCheck -check-prefix=CHECK-ARGC %s

// CHECK-ARGC: var C_ARGC: CInt

// CHECK-NOT: {{^}}import
// CHECK-NOT: @transparent
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

// CHECK-SUGAR: extension Array :
// CHECK-SUGAR: extension ImplicitlyUnwrappedOptional :
// CHECK-SUGAR: extension Optional :

// CHECK-MUTATING-ATTR: mutating func
