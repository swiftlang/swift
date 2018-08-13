// RUN: %empty-directory(%t/tmp)
// RUN: env TMPDIR=%t/tmp/ %target-build-swift -emit-module -o %t/test.swiftmodule %s
// RUN: env TMPDIR=%t/tmp/ %target-build-swift -g -v -o %t/sdk-link %s
// RUN: %target-codesign %t/sdk-link
// RUN: %target-run %t/sdk-link | %FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Foundation

// CHECK: {{^}}ABCDEF{{$}}
print(("ABC" as NSString).appending("DEF"))
