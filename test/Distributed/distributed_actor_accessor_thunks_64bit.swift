// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -module-name distributed_actor_accessors -emit-irgen -enable-experimental-distributed -disable-availability-checking -I %t 2>&1 %s | %IRGenFileCheck %s

// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed

// REQUIRES: CPU=x86_64

// UNSUPPORTED: OS=windows-msvc

import _Distributed
import FakeDistributedActorSystems

@available(SwiftStdlib 5.5, *)
typealias DefaultDistributedActorSystem = FakeActorSystem

enum SimpleE : Codable {
case a
}

enum E : Codable {
case a, b, c
}

enum IndirectE : Codable {
  case empty
  indirect case test(_: Int)
}

final class Obj : Codable, Sendable {
  let x: Int

  init(x: Int) {
    self.x = x
  }
}

struct LargeStruct : Codable {
  var a: Int
  var b: Int
  var c: String
  var d: Double
}

@available(SwiftStdlib 5.6, *)
public distributed actor MyActor {
  distributed func simple1(_: Int) {
  }

  // `String` would be a direct result as a struct type
  distributed func simple2(_: Int) -> String {
    return ""
  }

  // `String` is an object that gets exploded into two parameters
  distributed func simple3(_: String) -> Int {
    return 42
  }

  // Enum with a single case are special because they have an empty
  // native schema so they are dropped from parameters/result.
  distributed func single_case_enum(_ e: SimpleE) -> SimpleE {
    return e
  }

  distributed func with_indirect_enums(_: IndirectE, _: Int) -> IndirectE {
    return .empty
  }

  // Combination of multiple arguments, reference type and indirect result
  //
  // Note: Tuple types cannot be used here is either position because they
  // cannot conform to protocols.
  distributed func complex(_: [Int], _: Obj, _: String?, _: LargeStruct) -> LargeStruct {
    fatalError()
  }
}

@available(SwiftStdlib 5.6, *)
public distributed actor MyOtherActor {
  distributed func empty() {
  }
}

/// ---> Thunk and distributed method accessor for `simple1`

/// Let's make sure that accessor loads the data from the buffer and calls expected accessor

// CHECK: define hidden swifttailcc void @"$s27distributed_actor_accessors7MyActorC7simple1yySiFTE"

// CHECK: define internal swifttailcc void @"$s27distributed_actor_accessors7MyActorC7simple1yySiFTETF"(%swift.context* swiftasync %0, i8* %1, i8* %2, %T27distributed_actor_accessors7MyActorC* %3)

/// Read the current offset and cast an element to `Int`

// CHECK: store i8* %1, i8** %offset
// CHECK-NEXT: %elt_offset = load i8*, i8** %offset
// CHECK-NEXT: [[ELT_PTR:%.*]] = bitcast i8* %elt_offset to %TSi*
// CHECK-NEXT: [[NATIVE_VAL_LOC:%.*]] = getelementptr inbounds %TSi, %TSi* [[ELT_PTR]], i32 0, i32 0
// CHECK-NEXT: [[ARG_VAL:%.*]] = load i64, i64* [[NATIVE_VAL_LOC]]

/// Setup task context for async call to `simple1` thunk

// CHECK: [[CONTEXT_SIZE:%.*]] = load i32, i32* getelementptr inbounds (%swift.async_func_pointer, %swift.async_func_pointer* @"$s27distributed_actor_accessors7MyActorC7simple1yySiFTETu", i32 0, i32 1)
// CHECK-NEXT: [[CONTEXT_SIZE_64:%.*]] = zext i32 [[CONTEXT_SIZE]] to i64
// CHECK-NEXT: [[THUNK_ASYNC_CONTEXT:%.*]] = call swiftcc i8* @swift_task_alloc(i64 [[CONTEXT_SIZE_64]])
// CHECK: [[THUNK_CONTEXT_PTR:%.*]] = bitcast i8* [[THUNK_ASYNC_CONTEXT]] to %swift.context*

/// Call distributed thunk for `simple1` and `end` async context without results

// CHECK: [[THUNK_RESULT:%.*]] = call { i8*, %swift.error* } (i32, i8*, i8*, ...) @llvm.coro.suspend.async.sl_p0i8p0s_swift.errorss(
// CHECK-SAME: i8* bitcast (void (%swift.context*, i64, %T27distributed_actor_accessors7MyActorC*)* @"$s27distributed_actor_accessors7MyActorC7simple1yySiFTE" to i8*),
// CHECK-SAME: %swift.context* [[THUNK_CONTEXT_PTR]],
// CHECK-SAME: i64 [[ARG_VAL]],
// CHECK-SAME: %T27distributed_actor_accessors7MyActorC* %3)

// CHECK-NEXT: [[TASK_REF:%.*]] = extractvalue { i8*, %swift.error* } [[THUNK_RESULT]], 0
// CHECK-NEXT: {{.*}} = call i8* @__swift_async_resume_project_context(i8* [[TASK_REF]])
// CHECK: {{.*}} = call i1 (i8*, i1, ...) @llvm.coro.end.async({{.*}}, %swift.context* {{.*}}, %swift.error* {{.*}})

/// ---> Thunk and distributed method accessor for `simple2`

// CHECK: define hidden swifttailcc void @"$s27distributed_actor_accessors7MyActorC7simple2ySSSiFTE"

// CHECK: define internal swifttailcc void @"$s27distributed_actor_accessors7MyActorC7simple2ySSSiFTETF"

/// !!! - We are not going to double-check argument extraction here since it's the same as `simple1`.
// CHECK: [[NATIVE_ARG_VAL:%.*]] = load i64, i64* {{.*}}

/// Setup task context for async call to `simple2` thunk

// CHECK: [[CONTEXT_SIZE:%.*]] = load i32, i32* getelementptr inbounds (%swift.async_func_pointer, %swift.async_func_pointer* @"$s27distributed_actor_accessors7MyActorC7simple2ySSSiFTETu", i32 0, i32 1)
// CHECK-NEXT: [[CONTEXT_SIZE_64:%.*]] = zext i32 [[CONTEXT_SIZE]] to i64
// CHECK-NEXT: [[THUNK_ASYNC_CONTEXT:%.*]] = call swiftcc i8* @swift_task_alloc(i64 [[CONTEXT_SIZE_64]])
// CHECK: [[THUNK_CONTEXT_PTR:%.*]] = bitcast i8* [[THUNK_ASYNC_CONTEXT]] to %swift.context*

/// Call the thunk with extracted argument value

// CHECK: [[THUNK_RESULT:%.*]] = call { i8*, i64, %swift.bridge*, %swift.error* } (i32, i8*, i8*, ...) @llvm.coro.suspend.async.sl_p0i8i64p0s_swift.bridgesp0s_swift.errorss(
// CHECK-SAME: i8* bitcast (void (%swift.context*, i64, %T27distributed_actor_accessors7MyActorC*)* @"$s27distributed_actor_accessors7MyActorC7simple2ySSSiFTE" to i8*),
// CHECK-SAME: %swift.context* [[THUNK_CONTEXT_PTR]],
// CHECK-SAME: i64 [[NATIVE_ARG_VAL]],
// CHECK-SAME: %T27distributed_actor_accessors7MyActorC* %3)

// CHECK-NEXT: [[TASK_REF:%.*]] = extractvalue { i8*, i64, %swift.bridge*, %swift.error* } [[THUNK_RESULT]], 0
// CHECK-NEXT: {{.*}} = call i8* @__swift_async_resume_project_context(i8* [[TASK_REF]])

/// Extract information about `String` from result and call `end`

// CHECK: [[STR_STRUCT:%.*]] = insertvalue { i64, %swift.bridge* } {{.*}}, %swift.bridge* {{.*}}, 1
// CHECK: [[STR_SIZE:%.*]] = extractvalue { i64, %swift.bridge* } [[STR_STRUCT]], 0
// CHECK-NEXT: [[STR_VAL:%.*]] = extractvalue { i64, %swift.bridge* } [[STR_STRUCT]], 1

/// Initialize the result buffer with values produced by the thunk

// CHECK: store i64 [[STR_SIZE]], i64* %._guts._object._countAndFlagsBits._value
// CHECK: store %swift.bridge* [[STR_VAL]], %swift.bridge** %._guts._object._object

// CHECK: {{.*}} = call i1 (i8*, i1, ...) @llvm.coro.end.async({{.*}}, %swift.context* {{.*}}, %swift.error* {{.*}})

/// ---> Thunk and distributed method accessor for `simple3`

// CHECK: define hidden swifttailcc void @"$s27distributed_actor_accessors7MyActorC7simple3ySiSSFTE"

/// !!! in `simple3` interesting bits are: argument value extraction (because string is exploded into N arguments) and call to distributed thunk
// CHECK: define internal swifttailcc void @"$s27distributed_actor_accessors7MyActorC7simple3ySiSSFTETF"(%swift.context* swiftasync {{.*}}, i8* [[ARG_BUFF:%.*]], i8* [[RESULT_BUFF:%.*]], %T27distributed_actor_accessors7MyActorC* {{.*}})

// CHECK: [[TYPED_RESULT_BUFF:%.*]] = bitcast i8* [[RESULT_BUFF]] to %TSi*

// CHECK: [[ELT_PTR:%.*]] = bitcast i8* %elt_offset to %TSS*
// CHECK-NEXT: %._guts = getelementptr inbounds %TSS, %TSS* [[ELT_PTR]], i32 0, i32 0

// CHECK: [[STR_SIZE:%.*]] = load i64, i64* %._guts._object._countAndFlagsBits._value
// CHECK: [[STR_VAL:%.*]] = load %swift.bridge*, %swift.bridge** %._guts._object._object

/// Setup task context for async call to `simple3` thunk

// CHECK: [[CONTEXT_SIZE:%.*]] = load i32, i32* getelementptr inbounds (%swift.async_func_pointer, %swift.async_func_pointer* @"$s27distributed_actor_accessors7MyActorC7simple3ySiSSFTETu", i32 0, i32 1)
// CHECK-NEXT: [[CONTEXT_SIZE_64:%.*]] = zext i32 [[CONTEXT_SIZE]] to i64
// CHECK-NEXT: [[THUNK_ASYNC_CONTEXT:%.*]] = call swiftcc i8* @swift_task_alloc(i64 [[CONTEXT_SIZE_64]])
// CHECK: [[THUNK_CONTEXT_PTR:%.*]] = bitcast i8* [[THUNK_ASYNC_CONTEXT]] to %swift.context*

/// Call distributed thunk with exploaded string value

// CHECK: [[THUNK_RESULT:%.*]] = call { i8*, i64, %swift.error* } (i32, i8*, i8*, ...) @llvm.coro.suspend.async.sl_p0i8i64p0s_swift.errorss(
// CHECK-SAME: i8* bitcast (void (%swift.context*, i64, %swift.bridge*, %T27distributed_actor_accessors7MyActorC*)* @"$s27distributed_actor_accessors7MyActorC7simple3ySiSSFTE" to i8*),
// CHECK-SAME: %swift.context* [[THUNK_CONTEXT_PTR]],
// CHECK-SAME: i64 [[STR_SIZE]],
// CHECK-SAME: %swift.bridge* [[STR_VAL]],
// CHECK-SAME: %T27distributed_actor_accessors7MyActorC* %3)

// CHECK-NEXT: [[TASK_REF:%.*]] = extractvalue { i8*, i64, %swift.error* } [[THUNK_RESULT]], 0
// CHECK-NEXT: {{.*}} = call i8* @__swift_async_resume_project_context(i8* [[TASK_REF]])
// CHECK: [[INT_RES:%.*]] = extractvalue { i8*, i64, %swift.error* } [[THUNK_RESULT]], 1
// CHECK: %._value = getelementptr inbounds %TSi, %TSi* [[TYPED_RESULT_BUFF]], i32 0, i32 0
// CHECK: store i64 [[INT_RES]], i64* %._value
// CHECK: {{.*}} = call i1 (i8*, i1, ...) @llvm.coro.end.async({{.*}}, %swift.context* {{.*}}, %swift.error* {{.*}})

/// --> Thunk and distributed method accessor for `single_case_enum`

// CHECK: define hidden swifttailcc void @"$s27distributed_actor_accessors7MyActorC16single_case_enumyAA7SimpleEOAFFTE"

// CHECK: define internal swifttailcc void @"$s27distributed_actor_accessors7MyActorC16single_case_enumyAA7SimpleEOAFFTETF"(%swift.context* swiftasync %0, i8* [[BUFFER:%.*]], i8* [[RESULT_BUFF:%.*]], %T27distributed_actor_accessors7MyActorC* {{.*}})

/// First, let's check that there were no loads from the argument buffer and no stores to "current offset".

// CHECK: [[OFFSET:%.*]] = bitcast i8** %offset to i8*
// CHECK-NEXT: call void @llvm.lifetime.start.p0i8(i64 8, i8* [[OFFSET]])
// CHECK-NEXT: store i8* [[BUFFER]], i8** %offset
// CHECK-NEXT: %elt_offset = load i8*, i8** %offset
// CHECK-NEXT: [[ELT_PTR:.*]] = bitcast i8* %elt_offset to %T27distributed_actor_accessors7SimpleEO*
// CHECK-NEXT: [[OFFSET:%.*]] = bitcast i8** %offset to i8*
// CHECK-NEXT call void @llvm.lifetime.end.p0i8(i64 8, i8* [[OFFSET]])

/// Now, let's check that the call doesn't have any arguments and returns nothing.

// CHECK: [[THUNK_REF:%.*]] = bitcast void (%swift.context*, %T27distributed_actor_accessors7MyActorC*)* {{.*}} to i8*
// CHECK: {{.*}} = call { i8*, %swift.error* } (i32, i8*, i8*, ...) @llvm.coro.suspend.async.sl_p0i8p0s_swift.errorss({{.*}}, i8* [[THUNK_REF]], %swift.context* {{.*}}, %T27distributed_actor_accessors7MyActorC* {{.*}})
// CHECK: {{.*}} = call i1 (i8*, i1, ...) @llvm.coro.end.async({{.*}}, i8* {{.*}}, %swift.context* {{.*}}, %swift.error* {{.*}})

/// --> Thunk and distributed method accessor for `with_indirect_enums`

// CHECK: define hidden swifttailcc void @"$s27distributed_actor_accessors7MyActorC19with_indirect_enumsyAA9IndirectEOAF_SitFTE"
// CHECK: define internal swifttailcc void @"$s27distributed_actor_accessors7MyActorC19with_indirect_enumsyAA9IndirectEOAF_SitFTETF"

/// First, Load both arguments from the buffer.

// CHECK: [[TYPED_RESULT_BUFF:%.*]] = bitcast i8* %2 to %T27distributed_actor_accessors9IndirectEO*
// CHECK: store i8* %1, i8** %offset
// CHECK-NEXT: %elt_offset = load i8*, i8** %offset

// CHECK-NEXT: [[ENUM_PTR:%.*]] = bitcast i8* %elt_offset to %T27distributed_actor_accessors9IndirectEO*
// CHECK-NEXT: [[NATIVE_ENUM_PTR:%.*]] = bitcast %T27distributed_actor_accessors9IndirectEO* [[ENUM_PTR]] to i64*
// CHECK-NEXT: [[NATIVE_ENUM_VAL:%.*]] = load i64, i64* [[NATIVE_ENUM_PTR]]
// CHECK: [[ENUM_PTR_INT:%.*]] = ptrtoint %T27distributed_actor_accessors9IndirectEO* [[ENUM_PTR]] to i64
// CHECK-NEXT: [[NEXT_ELT_LOC:%.*]] = add i64 [[ENUM_PTR_INT]], 8
// CHECK-NEXT: [[NEXT_ELT_PTR:%.*]] = inttoptr i64 [[NEXT_ELT_LOC]] to i8*
// CHECK-NEXT: store i8* [[NEXT_ELT_PTR]], i8** %offset
// CHECK-NEXT: %elt_offset1 = load i8*, i8** %offset
// CHECK-NEXT: [[INT_PTR:%.*]] = bitcast i8* %elt_offset1 to %TSi*
// CHECK-NEXT: %._value = getelementptr inbounds %TSi, %TSi* [[INT_PTR]], i32 0, i32 0
// CHECK-NEXT: [[NATIVE_INT_VAL:%.*]] = load i64, i64* %._value

/// Call distributed thunk with extracted arguments.

// CHECK: [[THUNK_RESULT:%.*]] = call { i8*, i64, %swift.error* } (i32, i8*, i8*, ...) @llvm.coro.suspend.async.sl_p0i8i64p0s_swift.errorss({{.*}}, %swift.context* {{.*}}, i64 [[NATIVE_ENUM_VAL]], i64 [[NATIVE_INT_VAL]], %T27distributed_actor_accessors7MyActorC* {{.*}})
// CHECK-NEXT: [[TASK_REF:%.*]] = extractvalue { i8*, i64, %swift.error* } [[THUNK_RESULT]], 0
// CHECK-NEXT: {{.*}} = call i8* @__swift_async_resume_project_context(i8* [[TASK_REF]])
// CHECK: [[ENUM_RESULT:%.*]] = extractvalue { i8*, i64, %swift.error* } [[THUNK_RESULT]], 1
// CHECK: [[NATIVE_RESULT_PTR:%.*]] = bitcast %T27distributed_actor_accessors9IndirectEO* [[TYPED_RESULT_BUFF]] to i64*
// CHECK-NEXT: store i64 [[ENUM_RESULT]], i64* [[NATIVE_RESULT_PTR]]

// CHECK: {{.*}} = call i1 (i8*, i1, ...) @llvm.coro.end.async({{.*}}, %swift.context* {{.*}}, %swift.error* {{.*}})

/// ---> Thunk and distributed method for `complex`

// CHECK: define hidden swifttailcc void @"$s27distributed_actor_accessors7MyActorC7complexyAA11LargeStructVSaySiG_AA3ObjCSSSgAFtFTE"

// CHECK: define internal swifttailcc void @"$s27distributed_actor_accessors7MyActorC7complexyAA11LargeStructVSaySiG_AA3ObjCSSSgAFtFTETF"(%swift.context* swiftasync {{.*}}, i8* [[ARG_BUFF:%.*]], i8* [[RESULT_BUFF:%.*]], %T27distributed_actor_accessors7MyActorC* {{.*}})

/// First, let's check that all of the different argument types here are loaded correctly.

/// Cast result buffer to the expected result type (in this case its indirect opaque pointer)
// CHECK: [[TYPED_RESULT_BUFF:%.*]] = bitcast i8* [[RESULT_BUFF]] to %swift.opaque*

/// -> [Int]

// CHECK: %elt_offset = load i8*, i8** %offset
// CHECK-NEXT: [[ARR_PTR:%.*]] = bitcast i8* %elt_offset to %TSa*
// CHECK-NEXT: %._buffer = getelementptr inbounds %TSa, %TSa* [[ARR_PTR]], i32 0, i32 0
// CHECK: [[NATIVE_ARR_VAL:%.*]] = load [[ARR_STORAGE_TYPE:%.*]], [[ARR_STORAGE_TYPE]]* %._buffer._storage
// CHECK: [[ARR_PTR_INT:%.*]] = ptrtoint %TSa* [[ARR_PTR]] to i64
// CHECK-NEXT: [[NEXT_ELT:%.*]] = add i64 [[ARR_PTR_INT]], 8
// CHECK-NEXT: [[OPAQUE_NEXT_ELT:%.*]] = inttoptr i64 [[NEXT_ELT]] to i8*
// CHECK-NEXT: store i8* [[OPAQUE_NEXT_ELT]], i8** %offset

/// -> Obj

// CHECK-NEXT: %elt_offset1 = load i8*, i8** %offset
// CHECK-NEXT: [[OBJ_PTR:%.*]] = bitcast i8* %elt_offset1 to %T27distributed_actor_accessors3ObjC**
// CHECK-NEXT: [[NATIVE_OBJ_VAL:%.*]] = load %T27distributed_actor_accessors3ObjC*, %T27distributed_actor_accessors3ObjC** [[OBJ_PTR]]
// CHECK: [[OBJ_PTR_INT:%.*]] = ptrtoint %T27distributed_actor_accessors3ObjC** [[OBJ_PTR]] to i64
// CHECK-NEXT: [[NEXT_ELT:%.*]] = add i64 [[OBJ_PTR_INT]], 8
// CHECK-NEXT: [[NEXT_ELT_PTR:%.*]] = inttoptr i64 [[NEXT_ELT]] to i8*
// CHECK-NEXT: store i8* [[NEXT_ELT_PTR]], i8** %offset

/// -> String?

// CHECK-NEXT: %elt_offset2 = load i8*, i8** %offset
// CHECK-NEXT: [[OPT_PTR:%.*]] = bitcast i8* %elt_offset2 to %TSSSg*
// CHECK-NEXT: [[NATIVE_OPT_PTR:%.*]] = bitcast %TSSSg* [[OPT_PTR]] to { i64, i64 }*
// CHECK-NEXT: [[NATIVE_OPT_VAL_0_PTR:%.*]] = getelementptr inbounds { i64, i64 }, { i64, i64 }* [[NATIVE_OPT_PTR]], i32 0, i32 0
// CHECK-NEXT: [[NATIVE_OPT_VAL_0:%.*]] = load i64, i64* [[NATIVE_OPT_VAL_0_PTR]]
// CHECK-NEXT: [[NATIVE_OPT_VAL_1_PTR:%.*]] = getelementptr inbounds { i64, i64 }, { i64, i64 }* [[NATIVE_OPT_PTR]], i32 0, i32 1
// CHECK-NEXT: [[NATIVE_OPT_VAL_1:%.*]] = load i64, i64* [[NATIVE_OPT_VAL_1_PTR]]
// CHECK-NEXT: [[OPT_PTR_INT:%.*]] = ptrtoint %TSSSg* [[OPT_PTR]] to i64
// CHECK-NEXT: [[NEXT_ELT:%.*]] = add i64 [[OPT_PTR_INT]], 16
// CHECK-NEXT: [[NEXT_ELT_PTR:%.*]] = inttoptr i64 [[NEXT_ELT]] to i8*
// CHECK-NEXT: store i8* [[NEXT_ELT_PTR]], i8** %offset

/// -> LargeStruct (passed indirectly)

// CHECK-NEXT: %elt_offset3 = load i8*, i8** %offset
// CHECK-NEXT: [[STRUCT_PTR:%.*]] = bitcast i8* %elt_offset3 to %T27distributed_actor_accessors11LargeStructV*

// CHECK: [[INDIRECT_RESULT_BUFF:%.*]] = bitcast %swift.opaque* [[TYPED_RESULT_BUFF]] to %T27distributed_actor_accessors11LargeStructV*

/// Now let's make sure that distributed thunk call uses the arguments correctly

// CHECK: [[THUNK_RESULT:%.*]] = call { i8*, %swift.error* } (i32, i8*, i8*, ...) @llvm.coro.suspend.async.sl_p0i8p0s_swift.errorss({{.*}}, %T27distributed_actor_accessors11LargeStructV* [[INDIRECT_RESULT_BUFF]], %swift.context* {{.*}}, {{.*}} [[NATIVE_ARR_VAL]], %T27distributed_actor_accessors3ObjC* [[NATIVE_OBJ_VAL]], i64 [[NATIVE_OPT_VAL_0]], i64 [[NATIVE_OPT_VAL_1]], %T27distributed_actor_accessors11LargeStructV* [[STRUCT_PTR]], %T27distributed_actor_accessors7MyActorC* {{.*}})

/// RESULT is returned indirectly so there is nothing to pass to `end`

// CHECK: {{.*}} = call i1 (i8*, i1, ...) @llvm.coro.end.async({{.*}}, %swift.context* {{.*}}, %swift.error* {{.*}})

/// ---> Thunk and distributed method for `MyOtherActor.empty`

/// Let's check that there is no offset allocation here since parameter list is empty

// CHECK: define internal swifttailcc void @"$s27distributed_actor_accessors12MyOtherActorC5emptyyyFTETF"(%swift.context* swiftasync {{.*}}, i8* [[ARG_BUFF:%.*]], i8* [[RESULT_BUFF:%.*]], %T27distributed_actor_accessors12MyOtherActorC* {{.*}})
// CHECK-NEXT: entry:
// CHECK-NEXT: {{.*}} = alloca %swift.context*
// CHECK-NEXT: %swifterror = alloca swifterror %swift.error*
// CHECK-NEXT: {{.*}} = call token @llvm.coro.id.async(i32 20, i32 16, i32 0, i8* bitcast (%swift.async_func_pointer* @"$s27distributed_actor_accessors12MyOtherActorC5emptyyyFTETFTu" to i8*))
// CHECK-NEXT: {{.*}} = call i8* @llvm.coro.begin(token {{%.*}}, i8* null)
// CHECK-NEXT: store %swift.context* {{.*}}, %swift.context** {{.*}}
// CHECK-NEXT: store %swift.error* null, %swift.error** %swifterror
// CHECK-NEXT: {{.*}} = bitcast i8* [[RESULT_BUFF]] to %swift.opaque*
// CHECK-NEXT: {{.*}} = load i32, i32* getelementptr inbounds (%swift.async_func_pointer, %swift.async_func_pointer* @"$s27distributed_actor_accessors12MyOtherActorC5emptyyyFTETu", i32 0, i32 1)
