// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -emit-module-path %t/COM.swiftmodule -module-name COM %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -module-name M -sil-verify-all -emit-ir %s | %FileCheck %s --implicit-check-not=swift_getExistentialTypeMetadata
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -module-name M -sil-verify-all -enable-library-evolution -emit-ir %s | %FileCheck %s --implicit-check-not=swift_getExistentialTypeMetadata
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -module-name Interfaces -D LIBRARY -enable-library-evolution -emit-module-path %t/Interfaces.swiftmodule %s
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -module-name M -D CLIENT -sil-verify-all -emit-ir %s | %FileCheck %s --implicit-check-not=swift_getExistentialTypeMetadata
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -module-name M -sil-verify-all -O -emit-ir %s | %FileCheck %s --check-prefix=OPT

#if CLIENT
import Interfaces
#else
@com(interface: "10000000-0000-0000-0000-000000000001")
public protocol IBase {
  func method(_ value: CInt) -> CInt
  func output(_ value: UnsafeMutablePointer<CInt>)
}

@com(interface: "10000000-0000-0000-0000-000000000002")
public protocol IDerived: IBase {
  func derived(_ value: CInt) -> CInt
}

// Redundant ancestors and marker protocols do not add foreign slots.
@com(interface: "10000000-0000-0000-0000-000000000003")
public protocol ILeaf: IDerived, IBase, Sendable {
  func last() -> CInt
}

@com(interface: "10000000-0000-0000-0000-000000000004")
public protocol IProperties: AnyObject {
  var value: CInt { get set }
  subscript(_ index: CInt) -> CInt { get }
  func reset()
}
#endif

#if !LIBRARY

// CHECK-LABEL: define{{.*}} swiftcc i32 @"$s1M4base
// CHECK-SAME: (ptr [[ARG:%.*]], i32 [[VALUE:%.*]])
// CHECK: call void @llvm.lifetime.start{{.*}}(i64 {{4|8}}, ptr [[STORAGE:%.*]])
// CHECK-NEXT: store ptr [[ARG]], ptr [[STORAGE]]
// CHECK: [[INTERFACE:%.*]] = load ptr, ptr [[STORAGE]]
// CHECK: [[VTABLE:%.*]] = load ptr, ptr [[INTERFACE]]
// CHECK: [[SLOT:%.*]] = getelementptr inbounds ptr, ptr [[VTABLE]], i{{32|64}} 3
// CHECK: [[METHOD:%.*]] = load ptr, ptr [[SLOT]]
// CHECK: [[SELF:%.*]] = load ptr, ptr [[STORAGE]]
// CHECK: call {{(x86_stdcallcc )?}}i32 [[METHOD]](ptr [[SELF]], i32 [[VALUE]])
// CHECK: ret i32
public func base(_ interface: borrowing any IBase, _ value: CInt) -> CInt {
  interface.method(value)
}

// OPT-LABEL: define{{.*}} swiftcc i32 @"$s1M4base
// OPT-SAME: (ptr {{[^%]*}}[[SELF:%[^,) ]+]], i32 [[VALUE:%.*]])
// OPT: [[VTABLE:%.*]] = load ptr, ptr [[SELF]]
// OPT: [[SLOT:%.*]] = getelementptr inbounds{{.*}}, ptr [[VTABLE]], i{{32|64}} {{(3|12|24)}}
// OPT: [[METHOD:%.*]] = load ptr, ptr [[SLOT]]
// OPT: [[RESULT:%.*]] = {{(tail )?}}call {{(x86_stdcallcc )?}}i32 [[METHOD]](ptr {{[^%]*}}[[SELF]], i32 [[VALUE]])
// OPT: ret i32 [[RESULT]]

// CHECK-LABEL: define{{.*}} swiftcc i32 @"$s1M7refined
// CHECK-SAME: (ptr [[ARG:%.*]], i32 [[VALUE:%.*]])
// CHECK: call void @llvm.lifetime.start{{.*}}(i64 {{4|8}}, ptr [[STORAGE:%.*]])
// CHECK-NEXT: store ptr [[ARG]], ptr [[STORAGE]]
// CHECK: [[INTERFACE:%.*]] = load ptr, ptr [[STORAGE]]
// CHECK: [[VTABLE:%.*]] = load ptr, ptr [[INTERFACE]]
// CHECK: [[SLOT:%.*]] = getelementptr inbounds ptr, ptr [[VTABLE]], i{{32|64}} 3
// CHECK: [[METHOD:%.*]] = load ptr, ptr [[SLOT]]
// CHECK: [[SELF:%.*]] = load ptr, ptr [[STORAGE]]
// CHECK: call {{(x86_stdcallcc )?}}i32 [[METHOD]](ptr [[SELF]], i32 [[VALUE]])
// CHECK: ret i32
public func refined(_ interface: borrowing any IDerived, _ value: CInt) -> CInt {
  interface.method(value)
}

// CHECK-LABEL: define{{.*}} swiftcc i32 @"$s1M7derived
// CHECK-SAME: (ptr [[ARG:%.*]], i32 [[VALUE:%.*]])
// CHECK: call void @llvm.lifetime.start{{.*}}(i64 {{4|8}}, ptr [[STORAGE:%.*]])
// CHECK-NEXT: store ptr [[ARG]], ptr [[STORAGE]]
// CHECK: [[INTERFACE:%.*]] = load ptr, ptr [[STORAGE]]
// CHECK: [[VTABLE:%.*]] = load ptr, ptr [[INTERFACE]]
// CHECK: [[SLOT:%.*]] = getelementptr inbounds ptr, ptr [[VTABLE]], i{{32|64}} 5
// CHECK: [[METHOD:%.*]] = load ptr, ptr [[SLOT]]
// CHECK: [[SELF:%.*]] = load ptr, ptr [[STORAGE]]
// CHECK: call {{(x86_stdcallcc )?}}i32 [[METHOD]](ptr [[SELF]], i32 [[VALUE]])
// CHECK: ret i32
public func derived(_ interface: borrowing any IDerived, _ value: CInt) -> CInt {
  interface.derived(value)
}

// CHECK-LABEL: define{{.*}} swiftcc void @"$s1M6output
// CHECK: [[SLOT:%.*]] = getelementptr inbounds ptr, ptr {{%.*}}, i{{32|64}} 4
// CHECK: [[METHOD:%.*]] = load ptr, ptr [[SLOT]]
// CHECK: call {{(x86_stdcallcc )?}}void [[METHOD]](ptr {{%.*}}, ptr {{%.*}})
// CHECK: ret void
public func output(_ interface: borrowing any IBase, _ value: UnsafeMutablePointer<CInt>) {
  interface.output(value)
}

// CHECK-LABEL: define{{.*}} swiftcc i32 @"$s1M4leaf
// CHECK: [[SLOT:%.*]] = getelementptr inbounds ptr, ptr {{%.*}}, i{{32|64}} 6
// CHECK: [[METHOD:%.*]] = load ptr, ptr [[SLOT]]
// CHECK: call {{(x86_stdcallcc )?}}i32 [[METHOD]](ptr {{%.*}})
// CHECK: ret i32
public func leaf(_ interface: borrowing any ILeaf) -> CInt {
  interface.last()
}

// CHECK-LABEL: define{{.*}} swiftcc i32 @"$s1M4read
// CHECK-SAME: (ptr [[SELF:%[^,) ]+]]
// CHECK: [[SLOT:%.*]] = getelementptr inbounds ptr, ptr {{%.*}}, i{{32|64}} 3
// CHECK: [[METHOD:%.*]] = load ptr, ptr [[SLOT]]
// CHECK: call {{(x86_stdcallcc )?}}i32 [[METHOD]](ptr [[SELF]])
// CHECK: ret i32
public func read(_ interface: borrowing any IProperties) -> CInt {
  interface.value
}

// CHECK-LABEL: define{{.*}} swiftcc void @"$s1M5write
// CHECK-SAME: (ptr [[SELF:%[^,) ]+]]
// CHECK: [[SLOT:%.*]] = getelementptr inbounds ptr, ptr {{%.*}}, i{{32|64}} 4
// CHECK: [[METHOD:%.*]] = load ptr, ptr [[SLOT]]
// CHECK: call {{(x86_stdcallcc )?}}void [[METHOD]](ptr [[SELF]], i32 {{%.*}})
// CHECK: ret void
public func write(_ interface: borrowing any IProperties, _ value: CInt) {
  interface.value = value
}

// CHECK-LABEL: define{{.*}} swiftcc i32 @"$s1M13subscriptRead
// CHECK-SAME: (ptr [[SELF:%[^,) ]+]]
// CHECK: [[SLOT:%.*]] = getelementptr inbounds ptr, ptr {{%.*}}, i{{32|64}} 5
// CHECK: [[METHOD:%.*]] = load ptr, ptr [[SLOT]]
// CHECK: call {{(x86_stdcallcc )?}}i32 [[METHOD]](ptr [[SELF]], i32 {{%.*}})
// CHECK: ret i32
public func subscriptRead(_ interface: borrowing any IProperties, _ index: CInt) -> CInt {
  interface[index]
}

// CHECK-LABEL: define{{.*}} swiftcc void @"$s1M5reset
// CHECK-SAME: (ptr [[SELF:%[^,) ]+]]
// CHECK: [[SLOT:%.*]] = getelementptr inbounds ptr, ptr {{%.*}}, i{{32|64}} 6
// CHECK: [[METHOD:%.*]] = load ptr, ptr [[SLOT]]
// CHECK: call {{(x86_stdcallcc )?}}void [[METHOD]](ptr [[SELF]])
// CHECK: ret void
public func reset(_ interface: borrowing any IProperties) {
  interface.reset()
}

// The factory result keeps its COM cleanup through the opened temporary.
// CHECK-LABEL: define{{.*}} swiftcc i32 @"$s1M9temporary
// CHECK: [[OWNED:%.*]] = call swiftcc ptr {{%.*}}(
// CHECK: [[SLOT:%.*]] = getelementptr inbounds ptr, ptr {{%.*}}, i{{32|64}} 3
// CHECK: [[METHOD:%.*]] = load ptr, ptr [[SLOT]]
// CHECK: call {{(x86_stdcallcc )?}}i32 [[METHOD]](ptr {{%.*}}, i32 {{%.*}})
// CHECK: [[SLOT:%.*]] = getelementptr inbounds ptr, ptr {{%.*}}, i{{32|64}} 2
// CHECK: [[RELEASE:%.*]] = load ptr, ptr [[SLOT]]
// CHECK: call {{(x86_stdcallcc )?}}i32 [[RELEASE]](ptr {{%.*}})
// CHECK: ret i32
public func temporary(_ factory: () -> any IBase, _ value: CInt) -> CInt {
  factory().method(value)
}

// Compound accesses call the getter and setter, never a foreign coroutine.
// CHECK-LABEL: define{{.*}} swiftcc void @"$s1M9increment
// CHECK-SAME: (ptr [[SELF:%.*]])
// CHECK: [[GETSLOT:%.*]] = getelementptr inbounds ptr, ptr {{%.*}}, i{{32|64}} 3
// CHECK: [[GET:%.*]] = load ptr, ptr [[GETSLOT]]
// CHECK: call {{(x86_stdcallcc )?}}i32 [[GET]](ptr [[SELF]])
// CHECK: [[SETSLOT:%.*]] = getelementptr inbounds ptr, ptr {{%.*}}, i{{32|64}} 4
// CHECK: [[SET:%.*]] = load ptr, ptr [[SETSLOT]]
// CHECK: call {{(x86_stdcallcc )?}}void [[SET]](ptr [[SELF]], i32 {{%.*}})
// CHECK: ret void
public func increment(_ interface: borrowing any IProperties) {
  interface.value += 1
}

// The same getter/setter path also supports throwing inout accesses.
public func edit(_ interface: any IProperties,
                 _ body: (inout CInt) throws -> ()) rethrows {
  try body(&interface.value)
}
#endif
