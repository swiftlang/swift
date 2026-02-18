// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -enable-experimental-feature Embedded -c -I%t -parse-as-library %t/MyModule.swift -package-name P -o %t/MyModule.o -emit-module -emit-module-path %t/MyModule.swiftmodule -emit-empty-object-file
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -c -I%t %t/Main.swift -package-name P -o %t/Main.o
// RUN: %target-clang %target-clang-resource-dir-opt %t/Main.o %t/MyModule.o -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded

//--- MyModule.swift

public struct PublicNC: ~Copyable {
  public init() {}

  deinit {
    print("deinit PublicNC")
  }
}

package struct PackageNC: ~Copyable {
  package init() {}

  deinit {
    print("deinit PackageNC")
  }
}

package struct PackageGeneric<T>: ~Copyable {
  var t: T

  package init(_ t: T) { self.t = t }

  deinit {
    print("deinit PackageGeneric")
  }
}

struct InternalNC: ~Copyable {
  deinit {
    print("deinit InternalNC")
  }
}

private struct PrivateNC: ~Copyable {
  deinit {
    print("deinit PrivateNC")
  }
}

public func testInternal<T>(_ t: T) {
  _ = InternalNC()
}

public func testPrivate<T>(_ t: T) {
  _ = PrivateNC()
}

//--- Main.swift

import MyModule

@inline(never)
public func testPublic() {
  _ = PublicNC()
}

@inline(never)
public func testPackage() {
  _ = PackageNC()
}

@inline(never)
public func testPackageGeneric() {
  _ = PackageGeneric(27)
}

// CHECK: deinit PublicNC
testPublic()

// CHECK: deinit PackageNC
testPackage()

// CHECK: deinit PackageGeneric
testPackageGeneric()

// CHECK: deinit InternalNC
testInternal(0)

// CHECK: deinit PrivateNC
testPrivate(0)

