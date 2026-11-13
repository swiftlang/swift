// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/unsafe_swift_decls.swiftmodule %S/Inputs/unsafe_swift_decls.swift
// RUN: %target-swift-frontend -emit-module-path %t/safe_swift_decls.swiftmodule %S/Inputs/safe_swift_decls.swift -strict-memory-safety

// RUN: %target-typecheck-verify-swift -strict-memory-safety -I %S/Inputs -I %t -emit-loaded-module-trace-path %t/unsafe.trace

// RUN: %FileCheck -check-prefix TRACE %s < %t/unsafe.trace

// Enabling the diagnostics with '-Wwarning' also marks the module as strict.
// RUN: %empty-directory(%t/wwarning)
// RUN: %target-swift-frontend -emit-module-path %t/wwarning/safe_swift_decls.swiftmodule %S/Inputs/safe_swift_decls.swift -Wwarning StrictMemorySafety
// RUN: %target-typecheck-verify-swift -Wwarning StrictMemorySafety -I %S/Inputs -I %t/wwarning -I %t -emit-loaded-module-trace-path %t/wwarning.trace
// RUN: %FileCheck -check-prefix TRACE %s < %t/wwarning.trace

// File default doesn't make the module strict.
// RUN: %target-typecheck-verify-swift -D FILE_DEFAULT -I %S/Inputs -I %t -emit-loaded-module-trace-path %t/file_default.trace
// RUN: %FileCheck -check-prefix FILE-DEFAULT-TRACE %s < %t/file_default.trace

#if FILE_DEFAULT
default @diagnose(StrictMemorySafety, as: warning)
#endif

import unsafe_decls
import unsafe_swift_decls
import safe_swift_decls

// Module-level indication
// TRACE: "strictMemorySafety":true

// Dependencies
// TRACE-SAME: "safe_swift_decls"
// TRACE-SAME: "strictMemorySafety":true
// TRACE-SAME: "unsafe_swift_decls"
// TRACE-SAME: "strictMemorySafety":false

// FILE-DEFAULT-TRACE: "strictMemorySafety":false
// FILE-DEFAULT-TRACE-SAME: "safe_swift_decls"
// FILE-DEFAULT-TRACE-SAME: "strictMemorySafety":true
// FILE-DEFAULT-TRACE-SAME: "unsafe_swift_decls"
// FILE-DEFAULT-TRACE-SAME: "strictMemorySafety":false
