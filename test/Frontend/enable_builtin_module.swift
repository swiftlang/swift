// RUN: not %target-swift-frontend -typecheck -show-diagnostics-after-fatal -D BUILTIN_IMPORT %s 2>&1 | %FileCheck -check-prefix CHECK-NO-BUILTIN %s
// RUN: not %target-swift-frontend -typecheck -show-diagnostics-after-fatal  -enable-builtin-module %s 2>&1 | %FileCheck -check-prefix CHECK-NO-BUILTIN-IMPORT %s
// RUN: %target-swift-frontend -typecheck -enable-builtin-module -D BUILTIN_IMPORT %s
// RUN: %target-swift-frontend -typecheck -enable-experimental-feature BuiltinModule -D BUILTIN_IMPORT %s

// REQUIRES: swift_feature_BuiltinModule

// CHECK-NO-BUILTIN: no such module 'Builtin'

#if BUILTIN_IMPORT
import Builtin
#endif

// CHECK-NO-BUILTIN: cannot find type 'Builtin' in scope
// CHECK-NO-BUILTIN-IMPORT: cannot find type 'Builtin' in scope

func something(_: Builtin.RawPointer) {

}
