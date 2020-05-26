// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-as-library -emit-module -emit-module-path=%t/Module.swiftmodule -module-name=Module -DMODULE %s -O -emit-sil | %FileCheck %s

// Also link to make sure we don't eliminate any needed symbols.

// RUN: %target-swift-frontend -parse-as-library -emit-module -emit-module-path=%t/Module.swiftmodule -module-name=Module -DMODULE %s -O -c -o module.o
// RUN: %target-build-swift -DMAIN %s -I%t -O -o %t/a.out

#if MODULE

// Check if the optimizer eliminates testit() in Module.

// CHECK-NOT: {{sil .*testit.*}}

@_alwaysEmitIntoClient
@inline(never)
public func testit() {
  print("hello")
}

#endif


#if MAIN

import Module

public func caller() {
  testit()
}

#endif

