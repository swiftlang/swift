// RUN: %target-swift-frontend -target armv7-apple-none-macho -Xcc -D__MACH__ -emit-ir %s -enable-experimental-feature Embedded | %FileCheck %s
// RUN: %target-swift-frontend -target arm64-apple-none-macho -Xcc -D__MACH__ -Xcc -D__arm64__ -Xcc -D__APPLE__ -emit-ir %s -enable-experimental-feature Embedded | %FileCheck %s

// REQUIRES: VENDOR=apple

public func bool() -> Bool {
  return true
}

public func int() -> Int {
  return 42
}

public func ptr(p: UnsafeRawPointer, n: Int) -> UnsafeRawPointer {
  return p.advanced(by: n)
}

public func optional() -> Int? {
  return nil
}

public func staticstring() -> StaticString {
  return "hello"
}

public func checks(n: Int) {
  precondition(n > 0)
  precondition(n > 0, "message")
  assert(n > 0, "message")
  if n < 0 { fatalError() }
  if n < 0 { fatalError("with message") }
  if n < 0 { preconditionFailure() }
  if n < 0 { preconditionFailure("with message") }
  if n < 0 { assertionFailure() }
  if n < 0 { assertionFailure("with message") }
}

// CHECK: define {{.*}}i32 @main(i32 %0, ptr %1)
// CHECK: define {{.*}}i1 @"$s4main4boolSbyF"()
// CHECK: define {{.*}}i1 @"$sSb22_builtinBooleanLiteralSbBi1__tcfC"(i1 %0)
// CHECK: define {{.*}}{{i32|i64}} @"$s4main3intSiyF"()
// CHECK: define {{.*}}{{i32|i64}} @"$sSi22_builtinIntegerLiteralSiBI_tcfC"(ptr %0, {{i32|i64}} %1)
// CHECK: define {{.*}}ptr @"$s4main3ptr1p1nS2V_SitF"(ptr %0, {{i32|i64}} %1)
// CHECK: define {{.*}}ptr @"$sSV8advanced2bySVSi_tF"({{i32|i64}} %0, ptr %1)
// CHECK: define {{.*}}{ {{i32|i64}}, i8 } @"$s4main8optionalSiSgyF"()
// CHECK: define {{.*}}{ {{i32|i64}}, {{i32|i64}}, i8 } @"$s4main12staticstrings12StaticStringVyF"()
// CHECK: define {{.*}}{ {{i32|i64}}, {{i32|i64}}, i8 } @"$ss12StaticStringV08_builtinB7Literal17utf8CodeUnitCount7isASCIIABBp_BwBi1_tcfC"(ptr %0, {{i32|i64}} %1, i1 %2)
// CHECK: define {{.*}}void @"$s4main6checks1nySi_tF"({{i32|i64}} %0)
