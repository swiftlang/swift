// RUN: not %target-swift-frontend -parse %s -sdk "" 2>&1 | FileCheck -check-prefix=CHECK -check-prefix=NO-MODULE %s
// RUN: not %target-swift-frontend -parse %s -resource-dir / 2>&1 | FileCheck -check-prefix=CHECK -check-prefix=NO-STDLIB %s

// NO-MODULE: error: no such module 'NonExistent'
// NO-MODULE: note: did you forget to set an SDK using -sdk or SDKROOT?
// NO-MODULE-NEXT: use "{{.+}}" to select the default OS X SDK installed with Xcode

// NO-STDLIB: error: unable to load standard library for target '{{.+-.+}}'

// CHECK-NOT: error
// CHECK-NOT: warning
// CHECK-NOT: note
import NonExistent

// No subsequent diagnostics after fatal errors.
var x: NonExistent.Something
