// RUN: %empty-directory(%t)
// RUN: not %target-swift-frontend -typecheck %S/../../Inputs/empty.swift -enable-objc-interop -import-objc-header %t/fake.h -diagnostic-style llvm 2>&1 | %FileCheck -check-prefix=MISSING-HEADER %s

// RUN: cp %S/Inputs/error-on-define.h %t
// RUN: not %target-swift-frontend -typecheck %S/../../Inputs/empty.swift -enable-objc-interop -import-objc-header %t/error-on-define.h -diagnostic-style llvm 2>&1 | %FileCheck -check-prefix=MISSING-OTHER-HEADER %s

// RUN: cp %S/Inputs/error-on-define-impl.h %t
// RUN: not %target-swift-frontend -typecheck %S/../../Inputs/empty.swift -enable-objc-interop -import-objc-header %t/error-on-define.h -Xcc -DERROR -diagnostic-style llvm 2>&1 | %FileCheck -check-prefix=HEADER-ERROR %s


// RUN: %target-swift-frontend -emit-module -o %t -module-name HasBridgingHeader %S/../../Inputs/empty.swift -enable-objc-interop -import-objc-header %t/error-on-define.h -diagnostic-style llvm

// RUN: %target-swift-frontend -typecheck %s -I %t -Xcc -DERROR -verify -show-diagnostics-after-fatal
// RUN: not %target-swift-frontend -typecheck %s -I %t -Xcc -DERROR 2>&1 -diagnostic-style llvm | %FileCheck -check-prefix=HEADER-ERROR %s

// RUN: rm %t/error-on-define-impl.h
// RUN: %target-swift-frontend -typecheck %s -I %t -verify -show-diagnostics-after-fatal -diagnostic-style llvm
// RUN: not %target-swift-frontend -typecheck %s -I %t -diagnostic-style llvm 2>&1 | %FileCheck -check-prefix=MISSING-OTHER-HEADER %s

import HasBridgingHeader // expected-error {{failed to import bridging header}} expected-error {{failed to load module 'HasBridgingHeader'}}

// MISSING-HEADER: error: bridging header '{{.*}}/fake.h' does not exist
// MISSING-HEADER-NOT: error:

// MISSING-OTHER-HEADER: error: 'error-on-define-impl.h' file not found
// MISSING-OTHER-HEADER-NOT: error:
// MISSING-OTHER-HEADER: error: failed to import bridging header '{{.*}}/error-on-define.h'
// MISSING-OTHER-HEADER-NOT: error:

// HEADER-ERROR: error: "badness"
// HEADER-ERROR-NOT: error:
// HEADER-ERROR: error: failed to import bridging header '{{.*}}/error-on-define.h'
// HEADER-ERROR-NOT: error:

let _ = x // expected-error {{cannot find 'x' in scope}}
