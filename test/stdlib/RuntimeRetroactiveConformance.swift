// RUN: %empty-directory(%t)
// RUN: %target-build-swift -c -force-single-frontend-invocation -parse-as-library -emit-module -emit-module-path %t/A.swiftmodule -o %t/A.o -module-name A %S/Inputs/RuntimeRetroactiveConformance/A.swift 
// RUN: %target-build-swift -c -force-single-frontend-invocation -parse-as-library -emit-module -emit-module-path %t/B.swiftmodule -o %t/B.o -module-name B %S/Inputs/RuntimeRetroactiveConformance/B.swift 
// RUN: %target-build-swift %s %t/A.o %t/B.o -I %t -o %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test

import A
import B

extension AType: BProto {}

struct Foo<T: BProto> {}

struct Bar {
  var foo: Foo<AType> = Foo()
}

let mirror = Mirror(reflecting: Bar())

_ = mirror.children.first!

print("I survived") // CHECK: I survived
