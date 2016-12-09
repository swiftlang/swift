// RUN: rm -rf %t && mkdir -p %t
// RUN: not %target-swift-frontend -typecheck %S/../../Inputs/empty.swift -import-objc-header %t/fake.h 2>&1 | %FileCheck -check-prefix=MISSING-HEADER %s

// RUN: cp %S/Inputs/error-on-define.h %t
// RUN: not %target-swift-frontend -typecheck %S/../../Inputs/empty.swift -import-objc-header %t/error-on-define.h 2>&1 | %FileCheck -check-prefix=MISSING-OTHER-HEADER %s

// RUN: cp %S/Inputs/error-on-define-impl.h %t
// RUN: not %target-swift-frontend -typecheck %S/../../Inputs/empty.swift -import-objc-header %t/error-on-define.h -Xcc -DERROR 2>&1 | %FileCheck -check-prefix=HEADER-ERROR %s


// RUN: %target-swift-frontend -emit-module -o %t -module-name HasBridgingHeader %S/../../Inputs/empty.swift -import-objc-header %t/error-on-define.h

// RUN: %target-swift-frontend -typecheck %s -I %t -Xcc -DERROR -verify -show-diagnostics-after-fatal
// RUN: not %target-swift-frontend -typecheck %s -I %t -Xcc -DERROR 2>&1 | %FileCheck -check-prefix=HEADER-ERROR %s

// RUN: rm %t/error-on-define-impl.h
// RUN: %target-swift-frontend -typecheck %s -I %t -verify -show-diagnostics-after-fatal
// RUN: not %target-swift-frontend -typecheck %s -I %t 2>&1 | %FileCheck -check-prefix=MISSING-OTHER-HEADER %s

// REQUIRES: objc_interop

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

let _ = x // expected-error {{use of unresolved identifier 'x'}}
