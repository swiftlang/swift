// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-as-library -module-name=test -O %s -emit-ir -disable-reflection-metadata > %t/out.txt
// RUN: %FileCheck %s < %t/out.txt
// RUN: %FileCheck %s --check-prefix=NEGATIVE < %t/out.txt

// We have an unused conformance. It should not crash.
//
// FIXME: Actually stop emitting the conformance.
struct S { }

extension S: Equatable {
  static func ==(lhs: S, rhs: S) -> Bool {
    return false
  }
}

// We should only emit metadata for the second type, even though it has a
// field whose type is the first.
struct TypeOfProperty {
  var x: Int
}

struct HasPropertyType {
  var p: TypeOfProperty
}

// NEGATIVE-NOT: @"$s4test14TypeOfPropertyVMn"
// CHECK-LABEL: @"$s4test15HasPropertyTypeVMn"

@_optimize(none)
public func takeMetadata<T>(_: T.Type) {}

public func forceMetadata() {
  takeMetadata(HasPropertyType.self)
}

// CHECK-LABEL: @"$s4test1SVMn" = hidden constant
// CHECK-LABEL: @"$s4test1SVSQAAMc" = hidden constant
