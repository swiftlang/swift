// RUN: %target-swift-frontend -typecheck -I %S/Inputs -cxx-interoperability-mode=default -disable-availability-checking -Xcc -fdiagnostics-show-note-include-stack -diagnostic-style llvm %s 2>&1 | %FileCheck %s

import FunctionsAndMethodsReturningFRT

func testCaching() {
  _ = SourceLocationCaching.FactoryA.make()
  _ = SourceLocationCaching.FactoryB.make()
}

// CHECK: warning: '{{.*}}make{{.*}}' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it is returning a SWIFT_SHARED_REFERENCE
// CHECK-NOT: warning: '{{.*}}make{{.*}}' should be annotated with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED as it is returning a SWIFT_SHARED_REFERENCE
