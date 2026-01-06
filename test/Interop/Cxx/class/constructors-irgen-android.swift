// Target-specific tests for C++ constructor call code generation.

// RUN: %swift -module-name MySwift -target aarch64-unknown-linux-android -dump-clang-diagnostics -I %S/Inputs -enable-experimental-cxx-interop -emit-ir %s -parse-stdlib -parse-as-library -disable-legacy-type-info -Xcc -fignore-exceptions | %FileCheck %s -check-prefix=ITANIUM_ARM

// REQUIRES: OS=linux-android
// REQUIRES: CPU=aarch64

import Constructors
import TypeClassification

public func createHasVirtualBase() -> HasVirtualBase {
  // ITANIUM_ARM: define protected swiftcc void @"$s7MySwift20createHasVirtualBaseSo0deF0VyF"(ptr noalias sret({{.*}}) %0)
  // To verify that the thunk is inlined, make sure there's no intervening
  // `define`, i.e. the call to the C++ constructor happens in
  // createHasVirtualBase(), not some later function.
  // ITANIUM_ARM-NOT: define
  // Note `this` return type.
  // ITANIUM_ARM: call void @_ZN14HasVirtualBaseC1E7ArgType(ptr %{{[0-9]+}}, i64 %{{[0-9]+}})
  return HasVirtualBase(ArgType())
}

public func createImplicitDefaultConstructor() -> ImplicitDefaultConstructor {
  // ITANIUM_ARM: define protected swiftcc i32 @"$s7MySwift32createImplicitDefaultConstructorSo0deF0VyF"()
  // ITANIUM_ARM-NOT: define
  // Note `this` return type.
  // ITANIUM_ARM: call void @llvm.memset.p0.i64
  // ITANIUM_ARM: call void @_ZN26ImplicitDefaultConstructorC2Ev(ptr %{{[0-9]+}})
  return ImplicitDefaultConstructor()
}

public func createStructWithSubobjectCopyConstructorAndValue() {
  // ITANIUM_ARM-LABEL: define protected swiftcc void @"$s7MySwift48createStructWithSubobjectCopyConstructorAndValueyyF"()
  // ITANIUM_ARM: [[MEMBER:%.*]] = alloca %TSo33StructWithCopyConstructorAndValueV
  // ITANIUM_ARM: [[OBJ:%.*]] = alloca %TSo42StructWithSubobjectCopyConstructorAndValueV
  // ITANIUM_ARM: [[TMP:%.*]] = alloca %TSo33StructWithCopyConstructorAndValueV
  // ITANIUM_ARM: call void @_ZN33StructWithCopyConstructorAndValueC2Ev(ptr [[MEMBER]])
  // ITANIUM_ARM: call void @_ZN33StructWithCopyConstructorAndValueC2ERKS_(ptr [[TMP]], ptr [[MEMBER]])
  // ITANIUM_ARM: ret void
  let member = StructWithCopyConstructorAndValue()
  let obj = StructWithSubobjectCopyConstructorAndValue(member: member)
}

public func createTemplatedConstructor() {
  // ITANIUM_ARM-LABEL: define protected swiftcc void @"$s7MySwift26createTemplatedConstructoryyF"()
  // ITANIUM_ARM: [[OBJ:%.*]] = alloca %TSo20TemplatedConstructorV
  // ITANIUM_ARM: [[IVAL:%.*]] = load i64, ptr
  // ITANIUM_ARM: call void @_ZN20TemplatedConstructorC2I7ArgTypeEET_(ptr [[OBJ]], i64 [[IVAL]])
  // ITANIUM_ARM: ret void
  
  // ITANIUM_ARM-LABEL: define {{.*}}void @_ZN20TemplatedConstructorC2I7ArgTypeEET_(ptr {{.*}}, i64 {{.*}})
  let templated = TemplatedConstructor(ArgType())
}
