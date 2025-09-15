// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -parse-stdlib -enable-experimental-feature Embedded
// RUN: %target-swift-frontend -emit-ir     -I %t                      %t/Main.swift     -parse-stdlib -enable-experimental-feature Embedded | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

// BEGIN MyModule.swift

precedencegroup AssignmentPrecedence { assignment: true }

public struct NonGenericType {
  public init() {}
}

public struct GenericType<T> {
  var t: T
  public init(_ t: T) { self.t = t }
}

public protocol Protocol {}

public func nonGenericFunc() {
}

public func genericFunc<T>(_ t: T) -> T {
  return t
}

public func protocolBoundFunc<T: Protocol>(_ t: T) {

}

// BEGIN Main.swift

import MyModule

struct Bool {}

extension Bool: Protocol {}

extension GenericType: Protocol {}

public func main() {
  nonGenericFunc()
  _ = genericFunc(Bool())
  _ = NonGenericType()
  _ = GenericType<Bool>(Bool())
  protocolBoundFunc(Bool())
  protocolBoundFunc(GenericType<Bool>(Bool()))
}

// CHECK: define {{.*}}i32 @{{_*}}main{{.*}}(i32 %0, ptr %1)
// CHECK: define {{.*}}void @"$e4Main4BoolVACycfC"()
// CHECK: define {{.*}}void @"$e4Main4mainyyF"()
// CHECK: define {{.*}}void @"$e8MyModule14nonGenericFuncyyF"()
// CHECK: define {{.*}}void @"$e8MyModule11genericFuncyxxlF4Main4BoolV_Tg5"()
// CHECK: define {{.*}}void @"$e8MyModule14NonGenericTypeVACycfC"()
// CHECK: define {{.*}}void @"$e8MyModule11GenericTypeVyACyxGxcfC4Main4BoolV_Tt1g5"()
// CHECK: define {{.*}}void @"$e8MyModule17protocolBoundFuncyyxAA8ProtocolRzlF4Main4BoolV_Tg5"()
// CHECK: define {{.*}}void @"$e8MyModule17protocolBoundFuncyyxAA8ProtocolRzlFAA11GenericTypeVy4Main4BoolVG_Tg5"()
