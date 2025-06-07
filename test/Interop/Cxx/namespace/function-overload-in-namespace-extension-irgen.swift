// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-emit-ir -I %t/Inputs -enable-experimental-cxx-interop %t/test.swift -Xcc -fignore-exceptions | %FileCheck %t/test.swift


//--- Inputs/module.modulemap
module namespaces {
  header "test.h"
  requires cplusplus
}
//--- Inputs/test.h
namespace ExtendedInSwift {
void test(int, int);
} // namespace ExtendedInSwift

//--- test.swift

import namespaces;

extension ExtendedInSwift {
  static func test() {
  }
}

// CHECK: call void @{{.*}}(i32 0, i32 0)
// CHECK: call swiftcc void @"$sSo15ExtendedInSwiftO4testEACyyFZ"()
public func doTest() {
  ExtendedInSwift.test(0, 0)
  ExtendedInSwift.test()
}
