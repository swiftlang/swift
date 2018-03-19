// RUN: %swift -target thumbv7--windows-itanium -parse-stdlib -parse-as-library -module-name Swift -O -emit-ir %s -o - | %FileCheck %s

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

var gg = S()

public func f(s : S) -> (() -> ()) {
  return { gg = s }
}

// CHECK-DAG: @"\01l__swift4_reflection_descriptor" = private constant {{.*}}, section ".sw5cptr$B"
// CHECK-DAG: @"{{.*}}" = {{.*}} c"Sq", {{.*}} section ".sw5tyrf$B"
// CHECK-DAG: @{{[0-9]+}} = {{.*}} c"none\00", section ".sw5rfst$B"
// CHECK-DAG: @{{[0-9]+}} = {{.*}} c"some\00", section ".sw5rfst$B"
// CHECK-DAG: @"$SSqMF" = internal constant {{.*}}, section ".sw5flmd$B"
// CHECK-DAG: @"$Ss1SVs1PsMA" = internal constant {{.*}}, section ".sw5asty$B"
// CHECK-DAG: @"$SBoMB" = internal constant {{.*}}, section ".sw5bltn$B"

