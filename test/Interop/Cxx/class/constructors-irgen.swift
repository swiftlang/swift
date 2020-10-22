// Target-specific tests for C++ constructor call code generation.

// RUN: %swift -module-name Swift -target x86_64-apple-macosx10.9 -dump-clang-diagnostics -I %S/Inputs -enable-cxx-interop -emit-ir %s -parse-stdlib -parse-as-library -disable-legacy-type-info | %FileCheck %s -check-prefix=ITANIUM_X64
// RUN: %swift -module-name Swift -target armv7-none-linux-androideabi -dump-clang-diagnostics -I %S/Inputs -enable-cxx-interop -emit-ir %s -parse-stdlib -parse-as-library -disable-legacy-type-info | %FileCheck %s -check-prefix=ITANIUM_ARM
// RUN: %swift -module-name Swift -target x86_64-unknown-windows-msvc -dump-clang-diagnostics -I %S/Inputs -enable-cxx-interop -emit-ir %s -parse-stdlib -parse-as-library -disable-legacy-type-info | %FileCheck %s -check-prefix=MICROSOFT_X64

import Constructors
import TypeClassification

typealias Void = ()
struct UnsafePointer<T> { }
struct UnsafeMutablePointer<T> { }

public func createHasVirtualBase() -> HasVirtualBase {
  // ITANIUM_X64: define swiftcc void @"$ss20createHasVirtualBaseSo0bcD0VyF"(%TSo14HasVirtualBaseV* noalias nocapture sret %0)
  // ITANIUM_X64-NOT: define
  // ITANIUM_X64: call void @_ZN14HasVirtualBaseC1E7ArgType(%struct.HasVirtualBase* %{{[0-9]+}}, i32 %{{[0-9]+}})
  //
  // ITANIUM_ARM: define protected swiftcc void @"$ss20createHasVirtualBaseSo0bcD0VyF"(%TSo14HasVirtualBaseV* noalias nocapture sret %0)
  // To verify that the thunk is inlined, make sure there's no intervening
  // `define`, i.e. the call to the C++ constructor happens in
  // createHasVirtualBase(), not some later function.
  // ITANIUM_ARM-NOT: define
  // Note `this` return type.
  // ITANIUM_ARM: call %struct.HasVirtualBase* @_ZN14HasVirtualBaseC1E7ArgType(%struct.HasVirtualBase* %{{[0-9]+}}, [1 x i32] %{{[0-9]+}})
  //
  // MICROSOFT_X64: define dllexport swiftcc void @"$ss20createHasVirtualBaseSo0bcD0VyF"(%TSo14HasVirtualBaseV* noalias nocapture sret %0)
  // MICROSOFT_X64-NOT: define
  // Note `this` return type and implicit "most derived" argument.
  // MICROSOFT_X64: call %struct.HasVirtualBase* @"??0HasVirtualBase@@QEAA@UArgType@@@Z"(%struct.HasVirtualBase* %{{[0-9]+}}, i32 %{{[0-9]+}}, i32 1)
  return HasVirtualBase(ArgType())
}

public func createImplicitDefaultConstructor() -> ImplicitDefaultConstructor {
  // ITANIUM_X64: define swiftcc i32 @"$ss32createImplicitDefaultConstructorSo0bcD0VyF"()
  // ITANIUM_X64-NOT: define
  // ITANIUM_X64: call void @_ZN26ImplicitDefaultConstructorC1Ev(%struct.ImplicitDefaultConstructor* %{{[0-9]+}})
  //
  // ITANIUM_ARM: define protected swiftcc i32 @"$ss32createImplicitDefaultConstructorSo0bcD0VyF"()
  // ITANIUM_ARM-NOT: define
  // Note `this` return type.
  // ITANIUM_ARM: call %struct.ImplicitDefaultConstructor* @_ZN26ImplicitDefaultConstructorC2Ev(%struct.ImplicitDefaultConstructor* %{{[0-9]+}})
  //
  // MICROSOFT_X64: define dllexport swiftcc i32 @"$ss32createImplicitDefaultConstructorSo0bcD0VyF"()
  // MICROSOFT_X64-NOT: define
  // Note `this` return type but no implicit "most derived" argument.
  // MICROSOFT_X64: call %struct.ImplicitDefaultConstructor* @"??0ImplicitDefaultConstructor@@QEAA@XZ"(%struct.ImplicitDefaultConstructor* %{{[0-9]+}})
  return ImplicitDefaultConstructor()
}

public func createStructWithSubobjectCopyConstructorAndValue() {
  // ITANIUM_X64-LABEL: define swiftcc void @"$ss48createStructWithSubobjectCopyConstructorAndValueyyF"()
  // ITANIUM_X64: [[MEMBER:%.*]] = alloca %TSo33StructWithCopyConstructorAndValueV
  // ITANIUM_X64: [[MEMBER_AS_STRUCT:%.*]] = bitcast %TSo33StructWithCopyConstructorAndValueV* %member to %struct.StructWithCopyConstructorAndValue*
  // ITANIUM_X64: void @_ZN33StructWithCopyConstructorAndValueC1Ev(%struct.StructWithCopyConstructorAndValue* [[MEMBER_AS_STRUCT]])
  // ITANIUM_X64: call %TSo33StructWithCopyConstructorAndValueV* @"$sSo33StructWithCopyConstructorAndValueVWOc"(%TSo33StructWithCopyConstructorAndValueV* [[MEMBER]], %TSo33StructWithCopyConstructorAndValueV* [[TMP:%.*]])
  // ITANIUM_X64: [[OBJ_MEMBER:%.*]] = getelementptr inbounds %TSo42StructWithSubobjectCopyConstructorAndValueV, %TSo42StructWithSubobjectCopyConstructorAndValueV* %obj, i32 0, i32 0
  // ITANIUM_X64: call %TSo33StructWithCopyConstructorAndValueV* @"$sSo33StructWithCopyConstructorAndValueVWOb"(%TSo33StructWithCopyConstructorAndValueV* [[TMP]], %TSo33StructWithCopyConstructorAndValueV* [[OBJ_MEMBER]])
  // ITANIUM_X64: ret void

  // ITANIUM_ARM-LABEL: define protected swiftcc void @"$ss48createStructWithSubobjectCopyConstructorAndValueyyF"()
  // ITANIUM_ARM: [[MEMBER:%.*]] = alloca %TSo33StructWithCopyConstructorAndValueV
  // ITANIUM_ARM: [[MEMBER_AS_STRUCT:%.*]] = bitcast %TSo33StructWithCopyConstructorAndValueV* %member to %struct.StructWithCopyConstructorAndValue*
  // ITANIUM_ARM: call %struct.StructWithCopyConstructorAndValue* @_ZN33StructWithCopyConstructorAndValueC2Ev(%struct.StructWithCopyConstructorAndValue* [[MEMBER_AS_STRUCT]])
  // ITANIUM_ARM: call %TSo33StructWithCopyConstructorAndValueV* @"$sSo33StructWithCopyConstructorAndValueVWOc"(%TSo33StructWithCopyConstructorAndValueV* [[MEMBER]], %TSo33StructWithCopyConstructorAndValueV* [[TMP:%.*]])
  // ITANIUM_ARM: [[OBJ_MEMBER:%.*]] = getelementptr inbounds %TSo42StructWithSubobjectCopyConstructorAndValueV, %TSo42StructWithSubobjectCopyConstructorAndValueV* %obj, i32 0, i32 0
  // ITANIUM_ARM: call %TSo33StructWithCopyConstructorAndValueV* @"$sSo33StructWithCopyConstructorAndValueVWOb"(%TSo33StructWithCopyConstructorAndValueV* [[TMP]], %TSo33StructWithCopyConstructorAndValueV* [[OBJ_MEMBER]])
  // ITANIUM_ARM: ret void

  // MICROSOFT_X64-LABEL: define dllexport swiftcc void @"$ss48createStructWithSubobjectCopyConstructorAndValueyyF"()
  // MICROSOFT_X64: [[MEMBER:%.*]] = alloca %TSo33StructWithCopyConstructorAndValueV
  // MICROSOFT_X64: [[MEMBER_AS_STRUCT:%.*]] = bitcast %TSo33StructWithCopyConstructorAndValueV* %member to %struct.StructWithCopyConstructorAndValue*
  // MICROSOFT_X64: call %struct.StructWithCopyConstructorAndValue* @"??0StructWithCopyConstructorAndValue@@QEAA@XZ"(%struct.StructWithCopyConstructorAndValue* [[MEMBER_AS_STRUCT]])
  // MICROSOFT_X64: call %TSo33StructWithCopyConstructorAndValueV* @"$sSo33StructWithCopyConstructorAndValueVWOc"(%TSo33StructWithCopyConstructorAndValueV* [[MEMBER]], %TSo33StructWithCopyConstructorAndValueV* [[TMP:%.*]])
  // MICROSOFT_X64: [[OBJ_MEMBER:%.*]] = getelementptr inbounds %TSo42StructWithSubobjectCopyConstructorAndValueV, %TSo42StructWithSubobjectCopyConstructorAndValueV* %obj, i32 0, i32 0
  // MICROSOFT_X64: call %TSo33StructWithCopyConstructorAndValueV* @"$sSo33StructWithCopyConstructorAndValueVWOb"(%TSo33StructWithCopyConstructorAndValueV* [[TMP]], %TSo33StructWithCopyConstructorAndValueV* [[OBJ_MEMBER]])
  // MICROSOFT_X64: ret void
  let member = StructWithCopyConstructorAndValue()
  let obj = StructWithSubobjectCopyConstructorAndValue(member: member)
}
