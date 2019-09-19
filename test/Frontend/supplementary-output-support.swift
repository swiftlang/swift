// Checks what happens when trying to emit various supplementary outputs from
// modes that don't support them. This isn't critical because the driver should
// never ask the frontend to do this, but it's nice for people writing tests.

// RUN: not %target-swift-frontend -typecheck -emit-module %s 2>&1 | %FileCheck -check-prefix=PARSE_NO_MODULE %s
// PARSE_NO_MODULE: error: this mode does not support emitting modules{{$}}

// RUN: not %target-swift-frontend -parse -emit-dependencies %s 2>&1 | %FileCheck -check-prefix=PARSE_NO_DEPS %s
// PARSE_NO_DEPS: error: this mode does not support emitting dependency files{{$}}
// RUN: not %target-swift-frontend -dump-ast -emit-dependencies %s 2>&1 | %FileCheck -check-prefix=DUMP_NO_DEPS %s
// DUMP_NO_DEPS: error: this mode does not support emitting dependency files{{$}}

// RUN: not %target-swift-frontend -parse -emit-reference-dependencies %s 2>&1 | %FileCheck -check-prefix=PARSE_NO_REFERENCE_DEPS %s
// PARSE_NO_REFERENCE_DEPS: error: this mode does not support emitting reference dependency files{{$}}
// RUN: not %target-swift-frontend -dump-ast -emit-reference-dependencies %s 2>&1 | %FileCheck -check-prefix=DUMP_NO_REFERENCE_DEPS %s
// DUMP_NO_REFERENCE_DEPS: error: this mode does not support emitting reference dependency files{{$}}
// RUN: not %target-swift-frontend -resolve-imports -emit-reference-dependencies %s 2>&1 | %FileCheck -check-prefix=RESOLVE_IMPORTS_NO_REFERENCE_DEPS %s
// RESOLVE_IMPORTS_NO_REFERENCE_DEPS: error: this mode does not support emitting reference dependency files{{$}}

// RUN: not %target-swift-frontend -parse -emit-objc-header %s 2>&1 | %FileCheck -check-prefix=PARSE_NO_OBJC_HEADER %s
// PARSE_NO_OBJC_HEADER: error: this mode does not support emitting Objective-C headers{{$}}
// RUN: not %target-swift-frontend -dump-ast -emit-objc-header %s 2>&1 | %FileCheck -check-prefix=DUMP_NO_OBJC_HEADER %s
// DUMP_NO_OBJC_HEADER: error: this mode does not support emitting Objective-C headers{{$}}
// RUN: not %target-swift-frontend -resolve-imports -emit-objc-header %s 2>&1 | %FileCheck -check-prefix=RESOLVE_IMPORTS_NO_OBJC_HEADER %s
// RESOLVE_IMPORTS_NO_OBJC_HEADER: error: this mode does not support emitting Objective-C headers{{$}}

// RUN: not %target-swift-frontend -parse -emit-module-interface-path %t %s 2>&1 | %FileCheck -check-prefix=PARSE_NO_INTERFACE %s
// PARSE_NO_INTERFACE: error: this mode does not support emitting module interface files{{$}}
// RUN: not %target-swift-frontend -emit-silgen -emit-module-interface-path %t %s 2>&1 | %FileCheck -check-prefix=SILGEN_NO_INTERFACE %s
// SILGEN_NO_INTERFACE: error: this mode does not support emitting module interface files{{$}}
