// RUN: %swift -target thumbv7--windows-itanium -parse-stdlib -parse-as-library -module-name Swift -O -emit-ir %s -o - | FileCheck %s

// REQUIRES: CODEGENERATOR=ARM

precedencegroup AssignmentPrecedence {
  assignment: true
}

public enum Optional<Wrapped> {
  case none
  case some(Wrapped)
}

public protocol P {
  associatedtype T
}

public struct S : P {
  public typealias T = Optional<S>
}

public func f(s : S) -> (() -> ()) {
  return { _ = s }
}

// CHECK-DAG: @"\01l__swift3_capture_descriptor" = private constant {{.*}}, section ".sw3cptr"
// CHECK-DAG: @{{[0-9]+}} = private constant [3 x i8] c"Sq\00", section ".sw3tyrf"
// CHECK-DAG: @{{[0-9]+}} = private constant [5 x i8] c"none\00", section ".sw3rfst"
// CHECK-DAG: @{{[0-9]+}} = private constant [5 x i8] c"some\00", section ".sw3rfst"
// CHECK-DAG: @"\01l__swift3_reflection_metadata" = private constant {{.*}}, section ".sw3flmd"
// CHECK-DAG: @"\01l__swift3_assocty_metadata" = private constant {{.*}}, section ".sw3asty"
// CHECK-DAG: @"\01l__swift3_builtin_metadata" = private constant {{.*}}, section ".sw3bltn"

