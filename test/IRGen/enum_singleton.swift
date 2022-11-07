// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s --check-prefix=CHECK
// RUN: %target-swift-frontend -emit-ir -O %s | %FileCheck %s --check-prefix=CHECK

public enum Payload {
  case c1(Bool)
  case c2(Bool)
  case c3(Any)
}

public enum SingletonEnum {
  case c1(Payload)
}

// CHECK: define internal i32 @"$s14enum_singleton13SingletonEnumOwug"
// CHECK:   ret i32 0
// CHECK: }

// CHECK: define internal void @"$s14enum_singleton13SingletonEnumOwup"
// CHECK:   ret void
// CHECK: }