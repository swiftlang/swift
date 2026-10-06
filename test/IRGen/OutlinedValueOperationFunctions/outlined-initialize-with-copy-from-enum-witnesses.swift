// RUN: %target-swift-frontend %s -Osize -emit-ir -disable-llvm-optzns -disable-type-layout -o %t/out.ll
// RUN: %FileCheck %s < %t/out.ll

class MyNontrivialType {}

public struct Payload {
  let a: MyNontrivialType
  let b: MyNontrivialType
  let c: MyNontrivialType
}

public enum SingletonEnum {
  case payload(Payload)
}

public enum SinglePayloadEnum {
  case payload(Payload)
  case empty
}

public struct ForwardingPayload {
  let value: MyNontrivialType
  let tag: Int
}

public enum ForwardingSinglePayloadEnum {
  case payload(ForwardingPayload)
  case empty
}

public struct GenericPayload<T> {
  let a: T
  let b: T
  let c: T
}

public enum AddressOnlyMultiPayloadEnum<T> {
  case a(GenericPayload<T>)
  case b(GenericPayload<T>)
  case empty
}

public func copySingletonEnum(_ value: SingletonEnum) -> SingletonEnum {
  value
}

public func copySinglePayloadEnum(
  _ value: SinglePayloadEnum
) -> SinglePayloadEnum {
  value
}

public func copyForwardingSinglePayloadEnum(
  _ value: ForwardingSinglePayloadEnum
) -> ForwardingSinglePayloadEnum {
  value
}

public func copyAddressOnlyMultiPayloadEnum<T>(
  _ value: AddressOnlyMultiPayloadEnum<T>
) -> AddressOnlyMultiPayloadEnum<T> {
  value
}

// The payload's witness must emit its own implementation directly.
// CHECK-LABEL: define internal ptr @"$s3out7PayloadVwcp"
// CHECK-NOT: call ptr @"$s{{.*}}WOc"
// CHECK: ret ptr

// A singleton enum delegates its payload copy to the outlined helper.
// CHECK-LABEL: define internal ptr @"$s3out13SingletonEnumOwcp"
// CHECK: call ptr @"$s3out7PayloadVWOc"
// CHECK: ret ptr

// The normal single-payload strategy delegates the populated case to the
// payload's outlined helper.
// CHECK-LABEL: define internal ptr @"$s3out17SinglePayloadEnumOwcp"
// CHECK: call ptr @"$s3out7PayloadVWOc"
// CHECK: ret ptr

// ForwardToPayload may likewise use the payload's outlined helper.
// CHECK-LABEL: define internal ptr @"$s3out27ForwardingSinglePayloadEnumOwcp"
// CHECK: call ptr @"$s3out17ForwardingPayloadVWOc"
// CHECK: ret ptr

// An address-only multi-payload enum delegates each populated case to the
// generic payload's outlined helper.
// CHECK-LABEL: define internal ptr @"$s3out27AddressOnlyMultiPayloadEnumOwcp"
// CHECK: call ptr @"$s3out14GenericPayloadVyxGlWOc"
// CHECK: call ptr @"$s3out14GenericPayloadVyxGlWOc"
// CHECK: ret ptr
