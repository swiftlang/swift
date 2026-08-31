// RUN: %target-swift-emit-ir %s -parse-stdlib -enable-experimental-feature Embedded -enable-experimental-feature TypedAllocation -target arm64-apple-macos99.99 -wmo | %FileCheck %s
// RUN: %target-swift-emit-ir %s -parse-stdlib -enable-experimental-feature Embedded -enable-experimental-feature TypedAllocation -target arm64-apple-macos99.99 -wmo | %FileCheck %s --check-prefix=UNSAFEMUTABLEPOINTERALLOC
// RUN: %target-swift-emit-ir %s -parse-stdlib -enable-experimental-feature Embedded -enable-experimental-feature TypedAllocation -target arm64-apple-macos99.99 -wmo | %FileCheck %s --check-prefix=ERRORBOX
// RUN: %target-swift-emit-ir %s -parse-stdlib -enable-experimental-feature Embedded -enable-experimental-feature TypedAllocation -target arm64-apple-macos99.99 -wmo | %FileCheck %s --check-prefix=COROFRAME

// REQUIRES: OS=macosx
// REQUIRES: SWIFT_STDLIB_ARCH=arm64
// REQUIRES: embedded_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_TypedAllocation

import Swift

// CHECK: define swiftcc ptr @"$e16typed_allocation7MyClassC1x1yACSi_SitcfC"(i64 [[P0:%.*]], i64 [[P1:%.*]], ptr swiftself [[PSELF:%.*]])
// CHECK:   call noalias ptr @swift_allocObjectTyped(ptr getelementptr inbounds (%swift.embedded_existential_type, ptr @"$e16typed_allocation7MyClassCMf", i32 0, i32 1), i64 {{.*}}, i64 {{.*}}, i64 [[MYCLASS_TYPEID:.*]])
// CHECK:   ret ptr {{%.*}}
// CHECK: }
//
// CHECK: define swiftcc void @"$e16typed_allocation7MyClassCfD"(ptr swiftself %0) #0 {
// CHECK:   %1 = call swiftcc ptr @"$e16typed_allocation7MyClassCfd"(ptr swiftself %0)
// CHECK:   call void @swift_deallocClassInstanceTyped(ptr %1, i64 {{.*}}, i64 {{.*}}, i64 [[MYCLASS_TYPEID]])
public class MyClass {
  let x: Int
  let y: Int

  init(x: Int, y: Int) {
    self.x = x
    self.y = y
  }
}

// CHECK-LABEL: define swiftcc ptr @"$e16typed_allocation12MyOtherClassC1x1yACSi_AA0cE0CtcfC"(i64 %0, ptr %1, ptr swiftself %2)
// CHECK:   call noalias ptr @swift_allocObjectTyped(ptr getelementptr inbounds (%swift.embedded_existential_type, ptr @"$e16typed_allocation12MyOtherClassCMf", i32 0, i32 1), i64 32, i64 7, i64 [[MYOTHERCLASS_TYPEID:.*]])
// CHECK:   ret ptr {{%.*}}
// CHECK: }
//
// CHECK-LABEL: define swiftcc void @"$e16typed_allocation12MyOtherClassCfD"(ptr swiftself %0)
// CHECK:   %1 = call swiftcc ptr @"$e16typed_allocation12MyOtherClassCfd"(ptr swiftself %0)
// CHECK:   call void @swift_deallocClassInstanceTyped(ptr %1, i64 {{.*}}, i64 {{.*}}, i64 [[MYOTHERCLASS_TYPEID]])
public class MyOtherClass {
  let x: Int
  let y: MyClass

  init(x: Int, y: MyClass) {
    self.x = x
    self.y = y
  }
}

// CHECK-LABEL: define swiftcc void @"$e16typed_allocation3runyyAA3RefCFyyXEfU0_"(ptr %0, ptr captures(none) dereferenceable(16) %1)
// CHECK: %2 = call {{.*}} ptr @swift_allocObjectTyped(ptr {{.*}}, i64 {{.*}}, i64 {{.*}}, i64 [[P_CAPTURE_TYPEID:.*]])
// CHECK: call void @swift_deallocUninitializedObjectTyped(ptr %2, i64 {{.*}}, i64 {{.*}}, i64 [[P_CAPTURE_TYPEID]])
public class Ref {}
struct Pair {
  var a: Int
  var b: Ref
}
func f(_ r: Ref) -> Pair? { return nil }
func run(_ r: Ref) {
  var gc: () -> () = {}
  ({
    guard var p = f(r) else { return }
    let c = { p.a += 1 }
    gc = c
  })()
  gc()
}
run(Ref())

// CHECK-LABEL: define swiftcc { ptr, ptr } @"$e16typed_allocation11makeClosure1x1yyycSi_AA7MyClassCtF"(i64 %0, ptr %1)
// CHECK:   call noalias ptr @swift_allocObjectTyped(ptr @metadata{{.*}}, i64 32, i64 7, i64 [[CLOSURE_CONTEXT_TYPEID:.*]])
// CHECK:   ret { ptr, ptr } {{%.*}}
// CHECK: }
//
// CHECK-LABEL: define internal swiftcc void @__swift_closure_destructor(ptr swiftself %0)
// CHECK:   call void @swift_deallocObjectTyped(ptr %0, i64 32, i64 7, i64 [[CLOSURE_CONTEXT_TYPEID]])
@inline(never)
public func makeClosure(x: Int, y: MyClass) -> () -> Void {
  return {
    _ = (MyClass(x: x, y: y.x), MyOtherClass(x: x, y: y))
  }
}

public indirect enum Indirect {
  case x(Int, Int)
  case y(Int, Int)
}

// TODO: For some boxes we can't compute the typed malloc ID, because some of the fields are opaque,
//       so this test would not pass right now.

// DISABLED-CHECK-LABEL: define swiftcc i64 @"$e16typed_allocation8makeEnum1x1y1bAA8IndirectOSi_SiSbtF"(i64 %0, i64 %1, i1 %2) #0 {
// DISABLED-CHECK: entry:
// DISABLED-CHECK:   br i1 {{%.*}}, label %[[L1:.*]], label %[[L2:.*]]
// DISABLED-CHECK: [[L1]]:
// DISABLED-CHECK:   call noalias ptr @swift_allocObjectTyped(ptr getelementptr inbounds (%swift.full_boxmetadata, ptr @metadata{{.*}}, i32 0, i32 2), i64 {{.*}}, i64 7, i64 {{.*}})
// DISABLED-CHECK: [[L2]]:
// DISABLED-CHECK:   call noalias ptr @swift_allocObjectTyped(ptr getelementptr inbounds (%swift.full_boxmetadata, ptr @metadata{{.*}}, i32 0, i32 2), i64 {{.*}}, i64 7, i64 {{.*}})
// DISABLED-CHECK:   ret i64 {{%.*}}
// DISABLED-CHECK: }
public func makeEnum(x: Int, y: Int, b: Bool) -> Indirect {
  if b {
    return .x(y, x)
  }

  return .y(x, y)
}

@inline(never)
public func allocateInts(_ count: Int) -> UnsafeMutablePointer<Int> {
  return UnsafeMutablePointer<Int>.allocate(capacity: count)
}

@inline(never)
public func deallocateInts(_ ptr: UnsafeMutablePointer<Int>) {
  ptr.deallocate()
}

@inline(never)
public func allocateMyClasses(_ count: Int) -> UnsafeMutablePointer<MyClass> {
  return UnsafeMutablePointer<MyClass>.allocate(capacity: count)
}

@inline(never)
public func deallocateMyClasses(_ ptr: UnsafeMutablePointer<MyClass>) {
  ptr.deallocate()
}

// UNSAFEMUTABLEPOINTERALLOC-DAG: define {{.*}}swiftcc ptr @"$eSp8allocate8capacitySpyxGSi_tFZ16typed_allocation7MyClassC_Tt0g5"
// UNSAFEMUTABLEPOINTERALLOC-DAG:   call noalias ptr @swift_allocRawTyped(i64 {{.*}}, i64 {{.*}}, i64 [[MYCLASS_ARRAY_TYPEID:.*]])

// UNSAFEMUTABLEPOINTERALLOC-DAG: define {{.*}}swiftcc ptr @"$eSp8allocate8capacitySpyxGSi_tFZSi_Tt0g5"
// UNSAFEMUTABLEPOINTERALLOC-DAG:   call noalias ptr @swift_allocRawTyped(i64 {{.*}}, i64 {{.*}}, i64 [[INT_ARRAY_TYPEID:.*]])

// UNSAFEMUTABLEPOINTERALLOC-DAG: define {{.*}}swiftcc void @"$eSp10deallocateyyF16typed_allocation7MyClassC_Tg5"
// UNSAFEMUTABLEPOINTERALLOC-DAG:   call void @swift_deallocRawTyped(ptr %0, i64 {{.*}}, i64 {{.*}}, i64 [[MYCLASS_ARRAY_TYPEID]])

// UNSAFEMUTABLEPOINTERALLOC-DAG: define {{.*}}swiftcc void @"$eSp10deallocateyyFSi_Tg5"
// UNSAFEMUTABLEPOINTERALLOC-DAG:   call void @swift_deallocRawTyped(ptr %0, i64 {{.*}}, i64 {{.*}}, i64 [[INT_ARRAY_TYPEID]])


// --- Boxed opaque existential cases (typed alloc_box) ---

public protocol Existential {}

// Bigger than the 3-word inline existential buffer, so boxing is required.
public struct NotInlineFixed: Existential {
  var x1: Double
  var x2: Double
  var x3: Double
  var x4: Double
}

public struct NotInlineFixed2: Existential {
  var y1: Double
  var y2: Double
  var y3: Double
  var y4: Double
}

public protocol SelfReturningThrows {
  func upgrade() throws -> Self
}

extension NotInlineFixed: SelfReturningThrows {
  public func upgrade() throws -> NotInlineFixed { self }
}

@inline(never)
public func useSelfReturningThrows(_ p: any SelfReturningThrows) {}


// emitAllocateBoxedOpaqueExistentialBuffer.
@inline(never)
public func abandonGeneric<T: SelfReturningThrows>(_ v: T) throws {
  try useSelfReturningThrows(v.upgrade())
}

@inline(never)
public func runAbandonGeneric() throws {
  try abandonGeneric(NotInlineFixed(x1: 1, x2: 2, x3: 3, x4: 4))
}
// CHECK-LABEL: define {{.*}} @"$e{{[0-9]+}}typed_allocation14abandonGeneric{{.*}}"
// CHECK:   [[CALL:%[0-9]+]] = call swiftcc { ptr, ptr } @swift_allocBoxTyped(ptr {{.*}}, i64 [[TYPEID:[0-9]+]])
// CHECK:   [[BOX:%[0-9]+]] = extractvalue { ptr, ptr } [[CALL]], 0
// CHECK:   store ptr [[BOX]], ptr [[BUFFER:%[0-9]+]]
// CHECK:   [[DESCRIPTOR_SLOT:%[0-9]+]] = getelementptr inbounds{{.*}} i8, ptr [[BUFFER]], {{i64|i32}} 8
// CHECK:   store i64 [[TYPEID]], ptr [[DESCRIPTOR_SLOT]]
// CHECK:   br i1 {{%[0-9]+}}, label %[[ERRBB:[0-9]+]], label %[[OKBB:[0-9]+]]
//
// CHECK: [[OKBB]]:
// CHECK:   call void @__swift_destroy_boxed_opaque_existential_1(ptr {{%[0-9]+}})
//
// CHECK: [[ERRBB]]:
// CHECK:   call void @__swift_deallocate_boxed_opaque_existential_1(ptr {{%[0-9]+}})


// getAllocateBoxedOpaqueExistentialBufferFunction.
@inline(never)
public func abandonExistential(_ p: any SelfReturningThrows) throws {
  try useSelfReturningThrows(p.upgrade())
}
// CHECK-LABEL: define {{.*}} @"$e{{[0-9]+}}typed_allocation18abandonExistential{{.*}}"
// CHECK:   call ptr @__swift_allocate_boxed_opaque_existential_1(ptr {{%[0-9]+}})
// CHECK-LABEL: define {{.*}} @__swift_allocate_boxed_opaque_existential_1(ptr %0)
// CHECK: allocateBox:
// CHECK:   call swiftcc { ptr, ptr } @swift_allocBoxTyped(ptr {{.*}}, i64 [[HEADERID:[0-9]+]])
// CHECK:   store i64 [[HEADERID]], ptr {{.*}}


// getDeallocateBoxedOpaqueExistentialBufferFunction.

// CHECK-LABEL: define {{.*}} @__swift_deallocate_boxed_opaque_existential_1(ptr %0)
// CHECK: deallocateBox:
// CHECK:   [[REFERENCE:%[0-9]+]] = load ptr, ptr {{.*}}
// CHECK:   [[DESCRIPTOR3:%[0-9]+]] = load i64, ptr {{.*}}
// CHECK:   call void @swift_deallocRawTyped(ptr [[REFERENCE]], {{i64|i32}} {{.*}}, {{i64|i32}} {{.*}}, i64 [[DESCRIPTOR3]])


// getDestroyBoxedOpaqueExistentialBufferFunction.

// CHECK-LABEL: define {{.*}} @__swift_destroy_boxed_opaque_existential_1(ptr %0)
// CHECK: outline:
// CHECK:   [[REF2:%[0-9]+]] = load ptr, ptr {{.*}}
// CHECK:   [[DESCRIPTOR4:%[0-9]+]] = load i64, ptr {{.*}}
// CHECK:   call void @swift_releaseBoxTyped(ptr [[REF2]], i64 [[DESCRIPTOR4]])


// initializeWithCopy (OpaqueExistentialTypeInfo).
@inline(never)
public func copyExistential() -> any Existential {
  let x: any Existential = NotInlineFixed(x1: 1, x2: 2, x3: 3, x4: 4)
  let y = x
  return y
}
// CHECK-LABEL: define {{.*}} @"$e{{[0-9]+}}typed_allocation11Existential_pWOc"(ptr %0, ptr %1)
// CHECK:   [[SRC_BUFFER:%[0-9]+]] = getelementptr inbounds{{.*}} %T{{[0-9]+}}typed_allocation11ExistentialP, ptr %0, i32 0, i32 0
// CHECK:   [[DEST_BUFFER:%[0-9]+]] = getelementptr inbounds{{.*}} %T{{[0-9]+}}typed_allocation11ExistentialP, ptr %1, i32 0, i32 0
// CHECK:   {{%[0-9]+}} = call ptr {{%.*}}(ptr noalias [[DEST_BUFFER]], ptr noalias [[SRC_BUFFER]], ptr {{%[0-9]+}})
// CHECK:   [[SRC_DESCRIPTOR_SLOT:%[0-9]+]] = getelementptr inbounds{{.*}} i8, ptr [[SRC_BUFFER]], {{i64|i32}} 8
// CHECK:   [[DESCRIPTOR:%[0-9]+]] = load i64, ptr [[SRC_DESCRIPTOR_SLOT]]
// CHECK:   [[DEST_DESCRIPTOR_SLOT:%[0-9]+]] = getelementptr inbounds{{.*}} i8, ptr [[DEST_BUFFER]], {{i64|i32}} 8
// CHECK:   store i64 [[DESCRIPTOR]], ptr [[DEST_DESCRIPTOR_SLOT]]

// getProjectBoxedOpaqueExistentialFunction.
public protocol MutableSelfSized {
  mutating func bump()
  var value: Double { get }
}
extension NotInlineFixed: MutableSelfSized {
  public mutating func bump() { x1 += 1 }
  public var value: Double { x1 }
}
@inline(never)
public func mutateSharedExistential() -> Double {
  var x: any MutableSelfSized = NotInlineFixed(x1: 1, x2: 2, x3: 3, x4: 4)
  let y = x
  x.bump()
  return x.value + y.value
}
// CHECK-LABEL: define {{.*}} @__swift_mutable_project_boxed_opaque_existential_1(ptr %0, ptr %1)
// CHECK: boxed:
// CHECK:   [[DESCRIPTOR_SLOT7:%[0-9]+]] = getelementptr inbounds{{.*}} i8, ptr %0, {{i64|i32}} 8
// CHECK:   [[DESCRIPTOR7:%[0-9]+]] = load i64, ptr [[DESCRIPTOR_SLOT7]]
// CHECK:   {{%[0-9]+}} = call swiftcc { ptr, ptr } @swift_makeBoxUniqueTyped(ptr %0, ptr %1, i64 {{.*}}, i64 [[DESCRIPTOR7]])


// getAssignBoxedOpaqueExistentialBufferFunction (match-outline).
@inline(never)
func genericAssign<T>(_ dest: inout T, _ value: T) {
  dest = value
}
@inline(never)
public func reassignSameType() -> any Existential {
  var y: any Existential = NotInlineFixed(x1: 1, x2: 2, x3: 3, x4: 4)
  genericAssign(&y, NotInlineFixed(x1: 5, x2: 6, x3: 7, x4: 8))
  return y
}
// CHECK-LABEL: define {{.*}} @__swift_assign_boxed_opaque_existential_1(ptr %0, ptr %1)
// CHECK: match-outline:
// CHECK:   [[OLD_DEST_REF:%[0-9]+]] = load ptr, ptr [[DEST_BUFFER4:%[0-9]+]]
// CHECK:   [[SRC_REF:%[0-9]+]] = load ptr, ptr [[SRC_BUFFER4:%[0-9]+]]
// CHECK:   call ptr @swift_retain(ptr returned [[SRC_REF]])
// CHECK:   [[OLD_DESCRIPTOR_SLOT:%[0-9]+]] = getelementptr inbounds{{.*}} i8, ptr [[DEST_BUFFER4]], {{i64|i32}} 8
// CHECK:   [[OLD_DESCRIPTOR:%[0-9]+]] = load i64, ptr [[OLD_DESCRIPTOR_SLOT]]
// CHECK:   call void @swift_releaseBoxTyped(ptr [[OLD_DEST_REF]], i64 [[OLD_DESCRIPTOR]])
// CHECK:   store ptr [[SRC_REF]], ptr [[DEST_BUFFER4]]
// CHECK:   [[SRC_DESCRIPTOR_SLOT:%[0-9]+]] = getelementptr inbounds{{.*}} i8, ptr [[SRC_BUFFER4]], {{i64|i32}} 8
// CHECK:   [[NEW_DESCRIPTOR:%[0-9]+]] = load i64, ptr [[SRC_DESCRIPTOR_SLOT]]
// CHECK:   [[NEW_DESCRIPTOR_SLOT:%[0-9]+]] = getelementptr inbounds{{.*}} i8, ptr [[DEST_BUFFER4]], {{i64|i32}} 8
// CHECK:   store i64 [[NEW_DESCRIPTOR]], ptr [[NEW_DESCRIPTOR_SLOT]]


// getAssignBoxedOpaqueExistentialBufferFunction (no-match/dest-outline)
@inline(never)
public func reassignDifferentType() -> any Existential {
  var y: any Existential = NotInlineFixed(x1: 1, x2: 2, x3: 3, x4: 4)
  genericAssign(&y, NotInlineFixed2(y1: 5, y2: 6, y3: 7, y4: 8))
  return y
}
// CHECK: dest-outline:
// CHECK:   [[OLD_DEST_REF2:%[0-9]+]] = load ptr, ptr [[DEST_BUFFER5:%[0-9]+]]
// CHECK:   [[OLD_DESCRIPTOR_SLOT2:%[0-9]+]] = getelementptr inbounds{{.*}} i8, ptr [[DEST_BUFFER5]], {{i64|i32}} 8
// CHECK:   [[OLD_DESCRIPTOR2:%[0-9]+]] = load i64, ptr [[OLD_DESCRIPTOR_SLOT2]]
// CHECK: dest-outline-src-outline:
// CHECK:   [[SRC_REF2:%[0-9]+]] = load ptr, ptr [[SRC_BUFFER5:%[0-9]+]]
// CHECK:   call ptr @swift_retain(ptr returned [[SRC_REF2]])
// CHECK:   store ptr [[SRC_REF2]], ptr [[DEST_BUFFER5]]
// CHECK:   [[SRC_DESCRIPTOR_SLOT2:%[0-9]+]] = getelementptr inbounds{{.*}} i8, ptr [[SRC_BUFFER5]], {{i64|i32}} 8
// CHECK:   [[NEW_DESCRIPTOR2:%[0-9]+]] = load i64, ptr [[SRC_DESCRIPTOR_SLOT2]]
// CHECK:   [[NEW_DESCRIPTOR_SLOT2:%[0-9]+]] = getelementptr inbounds{{.*}} i8, ptr [[DEST_BUFFER5]], {{i64|i32}} 8
// CHECK:   store i64 [[NEW_DESCRIPTOR2]], ptr [[NEW_DESCRIPTOR_SLOT2]]
// CHECK: dest-outline-cont:
// CHECK:   call void @swift_releaseBoxTyped(ptr [[OLD_DEST_REF2]], i64 [[OLD_DESCRIPTOR2]])


// --- Boxed Error existential cases (typed allocErrorBoxTyped / deallocErrorBoxTyped) ---

public struct SimpleError: Error {}

// emitBoxedExistentialContainerAllocation (swift_allocError).
@inline(never)
public func makeError() -> any Error {
  return SimpleError()
}

// ARC release path (_errorBoxDestroyImpl).
@inline(never)
public func dropError() {
  _ = makeError()
}

// ERRORBOX-DAG: define {{.*}} @swift_allocError(ptr %0, ptr %1, ptr %2, i1 %3)
// ERRORBOX-DAG:   call noalias ptr @swift_allocObjectTyped(ptr @_swift_embedded_error_metadata_storage, i64 {{.*}}, i64 {{.*}}, i64 [[ERRORBOX_HEADER_TYPEID:[0-9]+]])

// ERRORBOX-DAG: define {{.*}} @_swift_embedded_error_destroy_impl(ptr %0)
// ERRORBOX-DAG:   call void @swift_deallocObjectTyped(ptr %0, i64 {{.*}}, i64 {{.*}}, i64 [[ERRORBOX_HEADER_TYPEID]])

// emitBoxedExistentialContainerDeallocation (swift_deallocError).
// ERRORBOX-DAG: define {{.*}} @swift_deallocError(ptr %0, ptr %1)
// ERRORBOX-DAG:   call void @swift_deallocObjectTyped(ptr %0, i64 {{.*}}, i64 {{.*}}, i64 [[ERRORBOX_HEADER_TYPEID]])


// --- Yield-once coroutine frames (typed alloc for swift_coroFrameAllocTyped/DeallocTyped) ---

public struct CoroFrameLarge {
  var a: Int
  var b: Int
  var c: Int
  var d: Int
  var e: Int
  var f: Int
  var g: Int
  var h: Int
}

public final class CoroFrameHolder {
  var large: CoroFrameLarge

  init(_ l: CoroFrameLarge) { large = l }

  public var value: CoroFrameLarge {
    _read {
      yield large
    }
    _modify {
      yield &large
    }
  }
}
// COROFRAME-DAG: define {{.*}} @"[[CORO_READ:\$e[0-9]+typed_allocation15CoroFrameHolderC5valueAA0.*5LargeVvr]]"(ptr {{.*}}, ptr swiftself {{.*}})
// COROFRAME-DAG:   call ptr @swift_coroFrameAllocTyped(i64 {{.*}}, i64 [[CORO_TYPEID:[0-9]+]])
//
// COROFRAME-DAG: define {{.*}} @"[[CORO_READ]].resume.0"
// COROFRAME-DAG:   call void @swift_coroFrameDeallocTyped(ptr {{.*}}, i64 [[CORO_TYPEID]])
