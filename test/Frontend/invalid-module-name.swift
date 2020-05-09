// Tests what happens when an explicitly-specified or inferred module name isn't
// a valid Swift identifier.

// RUN: not %target-swift-frontend -emit-silgen -parse-as-library %S/Inputs/invalid-module-name.swift 2>&1 | %FileCheck -check-prefix=INVALID_MODULE_NAME %s
// INVALID_MODULE_NAME: error: module name "invalid-module-name" is not a valid identifier; use -module-name flag to specify an alternate name

// RUN: not %target-swift-frontend -emit-silgen -parse-as-library %S/Inputs/invalid.module.name.swift 2>&1 | %FileCheck -check-prefix=INVALID_MODULE_NAME2 %s
// INVALID_MODULE_NAME2: error: module name "invalid.module.name" is not a valid identifier; use -module-name flag to specify an alternate name

// RUN: not %target-swift-frontend -emit-silgen -parse-as-library %S/Inputs/invalid-module-name.swift -module-name still-invalid 2>&1 | %FileCheck -check-prefix=STILL_INVALID %s
// STILL_INVALID: error: module name "still-invalid" is not a valid identifier{{$}}

// These should succeed.
// RUN: %target-swift-frontend -emit-silgen %S/Inputs/invalid-module-name.swift > /dev/null
// RUN: %target-swift-frontend -emit-silgen -parse-as-library %S/Inputs/invalid-module-name.swift -module-name foo > /dev/null
// RUN: %target-swift-frontend -typecheck -parse-as-library %S/Inputs/invalid-module-name.swift -module-name foo
