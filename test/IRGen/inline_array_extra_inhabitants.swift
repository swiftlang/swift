// RUN: %target-swift-frontend -parse-stdlib -parse-as-library -module-name Swift -emit-irgen %s | %FileCheck %s --check-prefixes=CHECK,CHECK-%target-endian
// RUN: %if CODEGENERATOR=SystemZ %{ %swift -target s390x-unknown-linux-gnu -parse-stdlib -parse-as-library -module-name Swift -emit-irgen %s | %FileCheck %s --check-prefixes=CHECK,CHECK-big %}

// Define Int locally so the cross-target run needs no s390x standard library.
struct Int { var value: Builtin.Word }
precedencegroup AssignmentPrecedence {}

public enum Color { case red, green, blue }
public enum Colors {
  case two(Builtin.FixedArray<2, Color>)
  case empty
}

// Both targets store and test the empty case in the first element.
// CHECK-LABEL: define {{.*}} @storeEmpty(
// CHECK-little: store i16 3, ptr
// CHECK-big: store i16 768, ptr
@_silgen_name("storeEmpty")
public func storeEmpty(_ value: inout Colors) {
  value = .empty
}

// CHECK-LABEL: define {{.*}} @replaceEmpty(
// CHECK-little: [[MASKED:%.*]] = and i16 {{%.*}}, 255
// CHECK-little: icmp eq i16 [[MASKED]], 3
// CHECK-big: [[MASKED:%.*]] = and i16 {{%.*}}, -256
// CHECK-big: icmp eq i16 [[MASKED]], 768
@_silgen_name("replaceEmpty")
public func replaceEmpty(_ value: inout Colors, with replacement: Colors) {
  if case .empty = value { value = replacement }
}
