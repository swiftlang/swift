// RUN: %empty-directory(%t)
// RUN: %target-build-swift -c %s -parse-as-library -force-single-frontend-invocation -o %t/swift.o -emit-objc-header-path %t/swift.h

// RUN: %clang -c %target-cc-options -isysroot %sdk -Weverything -Werror -Wno-unused-macros -Wno-incomplete-module -fobjc-arc -fmodules %S/Inputs/arc-conventions.m -o %t/main.o -I %t
// RUN: %target-build-swift %t/swift.o %t/main.o -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %S/Inputs/arc-conventions.m

// RUN: %clang -c %target-cc-options -isysroot %sdk -Weverything -Werror -Wno-unused-macros -Wno-incomplete-module -fno-objc-arc -fmodules %S/Inputs/arc-conventions.m -o %t/main.o -I %t
// RUN: %target-build-swift %t/swift.o %t/main.o -o %t/main2
// RUN: %target-codesign %t/main2
// RUN: %target-run %t/main2 | %FileCheck %S/Inputs/arc-conventions.m

// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation

public class Test: NSObject {
  @objc public func initAllTheThings() -> AnyObject {
    print("method called")
    return "initialized" as NSString
  }

  deinit {
    print("deinitialized \(self)")
  }
}
