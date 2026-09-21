// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -module-name A -swift-version 5 -O -wmo %s -emit-ir | %FileCheck %s

// RUN: %empty-directory(%t)
// RUN: %target-build-swift -O -wmo -o %t/main %target-rpath(%t) %s -swift-version 5
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s --check-prefix=CHECK-EXEC

// REQUIRES: executable_test

// The opaque type descriptor of a dynamic replacement is referenced by the
// dynamic replacement records, which are emitted after lazy emission has
// finished. Make sure the descriptor and its accessor are emitted as
// definitions and not just forward-declared, which used to fail to link with
// -O -wmo when the opaque type's effective access is internal or lower.

public protocol P {
  func myValue() -> Int
}

extension Int: P {
  public func myValue() -> Int { return self }
}

public struct Container {
  dynamic var prop: some P { return 1 }
}

extension Container {
  @_dynamicReplacement(for: prop)
  var _r_prop: some P { return 2 }
}

public func useProp(_ c: Container) -> Int {
  return c.prop.myValue()
}

// The opaque type descriptor of the replaced declaration is a definition.
// CHECK-DAG: @"$s1A9ContainerV4propQrvpQOMQ" = {{(hidden|internal)}} constant

// The opaque type descriptor of the replacement is a definition.
// CHECK-DAG: @"$s1A9ContainerV7_r_propQrvpQOMQ" = {{(hidden|internal)}} constant

// The opaque type descriptor accessor of the replacement is a definition.
// CHECK-DAG: define{{.*}} swiftcc {{.*}}@"$s1A9ContainerV7_r_propQrvpQOMg"()

// CHECK-EXEC: 2
print(Container().prop.myValue())
