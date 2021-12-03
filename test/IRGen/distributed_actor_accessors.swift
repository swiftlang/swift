// RUN: %target-swift-frontend -emit-irgen %s -swift-version 5 -enable-experimental-distributed | %IRGenFileCheck %s

// UNSUPPORTED: back_deploy_concurrency
// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

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
  public typealias Transport = AnyActorTransport

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
  public typealias Transport = AnyActorTransport

  distributed func empty() {
  }
}


/// ---> Let's check that distributed accessors and thunks are emitted as accessible functions

/// -> `MyActor.simple1`
// CHECK:      @"$s27distributed_actor_accessors7MyActorC7simple1yySiFTEHF" = private constant
// CHECK-SAME: @"symbolic Si___________pIetMHygzo_ 27distributed_actor_accessors7MyActorC s5ErrorP"
// CHECK-SAME: @"$s27distributed_actor_accessors7MyActorC7simple1yySiFTETF"
// CHECK-SAME: , section "__TEXT, __swift5_acfunc, regular"

/// -> `MyActor.simple2`
// CHECK:      @"$s27distributed_actor_accessors7MyActorC7simple2ySSSiFTEHF" = private constant
// CHECK-SAME: @"symbolic Si_____SS______pIetMHygozo_ 27distributed_actor_accessors7MyActorC s5ErrorP"
// CHECK-SAME: @"$s27distributed_actor_accessors7MyActorC7simple2ySSSiFTETF"
// CHECK-SAME: , section "__TEXT, __swift5_acfunc, regular"

/// -> `MyActor.simple3`
// CHECK:      @"$s27distributed_actor_accessors7MyActorC7simple3ySiSSFTEHF" = private constant
// CHECK-SAME: @"symbolic SS_____Si______pIetMHggdzo_ 27distributed_actor_accessors7MyActorC s5ErrorP"
// CHECK-SAME: @"$s27distributed_actor_accessors7MyActorC7simple3ySiSSFTETF"
// CHECK-SAME: , section "__TEXT, __swift5_acfunc, regular"

/// -> `MyActor.single_case_enum`
// CHECK:      @"$s27distributed_actor_accessors7MyActorC16single_case_enumyAA7SimpleEOAFFTEHF" = private constant
// CHECK-SAME: @"symbolic __________AA______pIetMHygdzo_ 27distributed_actor_accessors7SimpleEO AA7MyActorC s5ErrorP"
// CHECK-SAME: @"$s27distributed_actor_accessors7MyActorC16single_case_enumyAA7SimpleEOAFFTETF"
// CHECK-SAME: , section "__TEXT, __swift5_acfunc, regular"

/// -> `MyActor.with_indirect_enums`
// CHECK:      @"$s27distributed_actor_accessors7MyActorC19with_indirect_enumsyAA9IndirectEOAF_SitFTEHF" = private constant
// CHECK-SAME: @"symbolic _____Si_____AA______pIetMHgygozo_ 27distributed_actor_accessors9IndirectEO AA7MyActorC s5ErrorP"
// CHECK-SAME: @"$s27distributed_actor_accessors7MyActorC19with_indirect_enumsyAA9IndirectEOAF_SitFTETF"
// CHECK-SAME: , section "__TEXT, __swift5_acfunc, regular"

/// -> `MyActor.complex`
// CHECK:      @"$s27distributed_actor_accessors7MyActorC7complexyAA11LargeStructVSaySiG_AA3ObjCSSSgAFtFTEHF" = private constant
// CHECK-SAME: @"symbolic SaySiG_____SSSg__________AD______pIetMHgggngrzo_ 27distributed_actor_accessors3ObjC AA11LargeStructV AA7MyActorC s5ErrorP"
// CHECK-SAME: @"$s27distributed_actor_accessors7MyActorC7complexyAA11LargeStructVSaySiG_AA3ObjCSSSgAFtFTETF"
// CHECK-SAME: , section "__TEXT, __swift5_acfunc, regular"

/// -> `MyOtherActor.empty`
// CHECK:      @"$s27distributed_actor_accessors12MyOtherActorC5emptyyyFTEHF" = private constant
// CHECK-SAME: @"symbolic ___________pIetMHgzo_ 27distributed_actor_accessors12MyOtherActorC s5ErrorP"
// CHECK-SAME: @"$s27distributed_actor_accessors12MyOtherActorC5emptyyyFTETF"
// CHECK-SAME: , section "__TEXT, __swift5_acfunc, regular"

// CHECK:      @llvm.used = appending global [{{.*}} x i8*] [
// CHECK-SAME: @"$s27distributed_actor_accessors7MyActorC7simple1yySiFTEHF"
// CHECK-SAME: @"$s27distributed_actor_accessors7MyActorC7simple2ySSSiFTEHF"
// CHECK-SAME: @"$s27distributed_actor_accessors7MyActorC7simple3ySiSSFTEHF"
// CHECK-SAME: @"$s27distributed_actor_accessors7MyActorC16single_case_enumyAA7SimpleEOAFFTEHF"
// CHECK-SAME: @"$s27distributed_actor_accessors7MyActorC19with_indirect_enumsyAA9IndirectEOAF_SitFTEHF"
// CHECK-SAME: @"$s27distributed_actor_accessors7MyActorC7complexyAA11LargeStructVSaySiG_AA3ObjCSSSgAFtFTEHF"
// CHECK-SAME: @"$s27distributed_actor_accessors12MyOtherActorC5emptyyyFTEHF"
// CHECK-SAME: ], section "llvm.metadata"

/// ---> Thunk and distributed method accessor for `simple1`

/// Let's make sure that accessor loads the data from the buffer and calls expected accessor

// CHECK: define hidden swifttailcc void @"$s27distributed_actor_accessors7MyActorC7simple1yySiFTE"

// CHECK: define internal swifttailcc void @"$s27distributed_actor_accessors7MyActorC7simple1yySiFTETF"(%swift.context* swiftasync %0, i8* %1, %swift.refcounted* swiftself %2)

/// Read the current offset and cast an element to `Int`

// CHECK: store i8* %1, i8** %offset, align 8
// CHECK-NEXT: %elt_offset = load i8*, i8** %offset, align 8
// CHECK-NEXT: [[ELT_PTR:%.*]] = bitcast i8* %elt_offset to %TSi*
// CHECK-NEXT: %argval = load %TSi, %TSi* [[ELT_PTR]], align 8

// CHECK: [[NATIVE_VAL_LOC:%.*]] = bitcast %TSi* %argval.coercion.coerced to i64*
// CHECK-NEXT: [[ARG_VAL:%.*]] = load i64, i64* [[NATIVE_VAL_LOC]], align 8

/// Retrieve an async pointer to the distributed thunk for `simple1`

// CHECK: [[THUNK_LOC:%.*]] = add i64 ptrtoint (void (%swift.context*, i64, %T27distributed_actor_accessors7MyActorC*)* @"$s27distributed_actor_accessors7MyActorC7simple1yySiFTE" to i64), {{.*}}
// CHECK-NEXT: [[OPAQUE_THUNK_PTR:%.*]] = inttoptr i64 [[THUNK_LOC]] to i8*
// CHECK-NEXT: [[THUNK_PTR:%.*]] = bitcast i8* [[OPAQUE_THUNK_PTR]] to void (%swift.context*, i64, %T27distributed_actor_accessors7MyActorC*)*

/// Call distributed thunk for `simple1` and `end` async context without results

// CHECK: [[THUNK_PTR_REF:%.*]] = bitcast void (%swift.context*, i64, %T27distributed_actor_accessors7MyActorC*)* [[THUNK_PTR]] to i8*
// CHECK-NEXT: [[THUNK_RESULT:%.*]] = call { i8*, %swift.error* } (i32, i8*, i8*, ...) @llvm.coro.suspend.async.sl_p0i8p0s_swift.errorss({{.*}}, i8* [[THUNK_PTR_REF]], %swift.context* {{.*}}, i64 [[ARG_VAL]], %T27distributed_actor_accessors7MyActorC* {{.*}})
// CHECK-NEXT: [[TASK_REF:%.*]] = extractvalue { i8*, %swift.error* } [[THUNK_RESULT]], 0
// CHECK-NEXT: {{.*}} = call i8* @__swift_async_resume_project_context(i8* [[TASK_REF]])
// CHECK: {{.*}} = call i1 (i8*, i1, ...) @llvm.coro.end.async({{.*}}, %swift.context* {{.*}}, %swift.error* {{.*}})

/// ---> Thunk and distributed method accessor for `simple2`

// CHECK: define hidden swifttailcc void @"$s27distributed_actor_accessors7MyActorC7simple2ySSSiFTE"

// CHECK: define internal swifttailcc void @"$s27distributed_actor_accessors7MyActorC7simple2ySSSiFTETF"

/// !!! - We are not going to double-check argument extraction here since it's the same as `simple1`.
// CHECK: [[NATIVE_ARG_VAL:%.*]] = load i64, i64* {{.*}}, align 8

/// Load async pointer to distributed thunk for `simple2`

// CHECK: [[THUNK_LOC:%.*]] = add i64 ptrtoint (void (%swift.context*, i64, %T27distributed_actor_accessors7MyActorC*)* @"$s27distributed_actor_accessors7MyActorC7simple2ySSSiFTE" to i64), {{.*}}
// CHECK-NEXT: [[OPAQUE_THUNK_PTR:%.*]] = inttoptr i64 [[THUNK_LOC]] to i8*
// CHECK-NEXT: [[THUNK_PTR:%.*]] = bitcast i8* [[OPAQUE_THUNK_PTR]] to void (%swift.context*, i64, %T27distributed_actor_accessors7MyActorC*)*

/// Call the thunk with extracted argument value

// CHECK: [[THUNK_PTR_REF:%.*]] = bitcast void (%swift.context*, i64, %T27distributed_actor_accessors7MyActorC*)* [[THUNK_PTR]] to i8*
// CHECK-NEXT: [[THUNK_RESULT:%.*]] = call { i8*, i64, %swift.bridge*, %swift.error* } (i32, i8*, i8*, ...) @llvm.coro.suspend.async.sl_p0i8i64p0s_swift.bridgesp0s_swift.errorss({{.*}}, i8* [[THUNK_PTR_REF]], %swift.context* {{.*}}, i64 [[NATIVE_ARG_VAL]], %T27distributed_actor_accessors7MyActorC* {{.*}})
// CHECK-NEXT: [[TASK_REF:%.*]] = extractvalue { i8*, i64, %swift.bridge*, %swift.error* } [[THUNK_RESULT]], 0
// CHECK-NEXT: {{.*}} = call i8* @__swift_async_resume_project_context(i8* [[TASK_REF]])

/// Extract information about `String` from result and call `end`

// CHECK: [[STR_STRUCT:%.*]] = insertvalue { i64, %swift.bridge* } {{.*}}, %swift.bridge* {{.*}}, 1
// CHECK: [[STR_SIZE:%.*]] = extractvalue { i64, %swift.bridge* } [[STR_STRUCT]], 0
// CHECK-NEXT: [[STR_VAL:%.*]] = extractvalue { i64, %swift.bridge* } [[STR_STRUCT]], 1
// CHECK: {{.*}} = call i1 (i8*, i1, ...) @llvm.coro.end.async({{.*}}, %swift.context* {{.*}}, i64 [[STR_SIZE]], %swift.bridge* [[STR_VAL]], %swift.error* {{.*}})

/// ---> Thunk and distributed method accessor for `simple3`

// CHECK: define hidden swifttailcc void @"$s27distributed_actor_accessors7MyActorC7simple3ySiSSFTE"

/// !!! in `simple3` interesting bits are: argument value extraction (because string is exploded into N arguments) and call to distributed thunk
// CHECK: define internal swifttailcc void @"$s27distributed_actor_accessors7MyActorC7simple3ySiSSFTETF"

// CHECK: %argval = load %TSS, %TSS* {{.*}}, align 8
// CHECK: [[NATIVE_STR_PTR:%.*]] = bitcast %TSS* %argval.coercion.coerced to { i64, %swift.bridge* }*
// CHECK-NEXT: [[NATIVE_STR:%.*]] = load { i64, %swift.bridge* }, { i64, %swift.bridge* }* [[NATIVE_STR_PTR]], align 8
// CHECK: [[STR_SIZE:%.*]] = extractvalue { i64, %swift.bridge* } [[NATIVE_STR]], 0
// CHECK-NEXT: [[STR_VAL:%.*]] = extractvalue { i64, %swift.bridge* } [[NATIVE_STR]], 1

/// Load pointer to a distributed thunk for `simple3`

// CHECK: [[THUNK_LOC:%.*]] = add i64 ptrtoint (void (%swift.context*, i64, %swift.bridge*, %T27distributed_actor_accessors7MyActorC*)* @"$s27distributed_actor_accessors7MyActorC7simple3ySiSSFTE" to i64), {{.*}}
// CHECK-NEXT: [[OPAQUE_THUNK_PTR:%.*]] = inttoptr i64 [[THUNK_LOC]] to i8*
// CHECK-NEXT: [[THUNK_PTR:%.*]] = bitcast i8* [[OPAQUE_THUNK_PTR]] to void (%swift.context*, i64, %swift.bridge*, %T27distributed_actor_accessors7MyActorC*)*

/// Call distributed thunk with exploaded string value

// CHECK: [[OPAQUE_THUNK_PTR:%.*]] = bitcast void (%swift.context*, i64, %swift.bridge*, %T27distributed_actor_accessors7MyActorC*)* [[THUNK_PTR]] to i8*
// CHECK-NEXT: [[THUNK_RESULT:%.*]] = call { i8*, i64, %swift.error* } (i32, i8*, i8*, ...) @llvm.coro.suspend.async.sl_p0i8i64p0s_swift.errorss({{.*}}, i8* [[OPAQUE_THUNK_PTR]], %swift.context* {{.*}}, i64 [[STR_SIZE]], %swift.bridge* [[STR_VAL]], %T27distributed_actor_accessors7MyActorC* {{.*}})
// CHECK-NEXT: [[TASK_REF:%.*]] = extractvalue { i8*, i64, %swift.error* } [[THUNK_RESULT]], 0
// CHECK-NEXT: {{.*}} = call i8* @__swift_async_resume_project_context(i8* [[TASK_REF]])
// CHECK: [[INT_RES:%.*]] = extractvalue { i8*, i64, %swift.error* } [[THUNK_RESULT]], 1
// CHECK: {{.*}} = call i1 (i8*, i1, ...) @llvm.coro.end.async({{.*}}, %swift.context* {{.*}}, i64 [[INT_RES]], %swift.error* {{.*}})

/// --> Thunk and distributed method accessor for `single_case_enum`

// CHECK: define hidden swifttailcc void @"$s27distributed_actor_accessors7MyActorC16single_case_enumyAA7SimpleEOAFFTE"

// CHECK: define internal swifttailcc void @"$s27distributed_actor_accessors7MyActorC16single_case_enumyAA7SimpleEOAFFTETF"(%swift.context* swiftasync %0, i8* [[BUFFER:%.*]], %swift.refcounted* swiftself %2)

/// First, let's check that there were no loads from the argument buffer and no stores to "current offset".

// CHECK: [[OFFSET:%.*]] = bitcast i8** %offset to i8*
// CHECK-NEXT: call void @llvm.lifetime.start.p0i8(i64 8, i8* [[OFFSET]])
// CHECK-NEXT: store i8* [[BUFFER]], i8** %offset, align 8
// CHECK-NEXT: %elt_offset = load i8*, i8** %offset, align 8
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

// CHECK: store i8* %1, i8** %offset, align 8
// CHECK-NEXT: %elt_offset = load i8*, i8** %offset, align 8
// CHECK-NEXT: [[ENUM_PTR:%.*]] = bitcast i8* %elt_offset to %T27distributed_actor_accessors9IndirectEO*
// CHECK-NEXT: %argval = load %T27distributed_actor_accessors9IndirectEO, %T27distributed_actor_accessors9IndirectEO* [[ENUM_PTR]], align 8
// CHECK-NEXT: [[OPAQUE_ENUM_PTR:%.*]] = bitcast %T27distributed_actor_accessors9IndirectEO* %argval.coercion.coerced to i8*
// CHECK: store %T27distributed_actor_accessors9IndirectEO %argval, %T27distributed_actor_accessors9IndirectEO* %argval.coercion.coerced, align 8
// CHECK-NEXT: [[COERCED_ENUM_PTR:%.*]] = bitcast %T27distributed_actor_accessors9IndirectEO* %argval.coercion.coerced to i64*
// CHECK-NEXT: [[NATIVE_ENUM_VAL:%.*]] = load i64, i64* [[COERCED_ENUM_PTR]], align 8
// CHECK: [[ENUM_PTR_INT:%.*]] = ptrtoint %T27distributed_actor_accessors9IndirectEO* [[ENUM_PTR]] to i64
// CHECK-NEXT: [[NEXT_ELT_LOC:%.*]] = add i64 [[ENUM_PTR_INT]], 8
// CHECK-NEXT: [[NEXT_ELT_PTR:%.*]] = inttoptr i64 %13 to i8*
// CHECK-NEXT: store i8* [[NEXT_ELT_PTR]], i8** %offset, align 8
// CHECK-NEXT: %elt_offset1 = load i8*, i8** %offset, align 8
// CHECK-NEXT: [[INT_PTR:%.*]] = bitcast i8* %elt_offset1 to %TSi*
// CHECK-NEXT: %argval2 = load %TSi, %TSi* [[INT_PTR]], align 8
// CHECK-NEXT: [[OPAQUE_INT_PTR:%.*]] = bitcast %TSi* %argval2.coercion.coerced to i8*
// CHECK: store %TSi %argval2, %TSi* %argval2.coercion.coerced, align 8
// CHECK-NEXT: [[COERCED_INT_PTR:%.*]] = bitcast %TSi* %argval2.coercion.coerced to i64*
// CHECK-NEXT: [[NATIVE_INT_VAL:%.*]] = load i64, i64* [[COERCED_INT_PTR]], align 8

/// Call distributed thunk with extracted arguments.

// CHECK: [[THUNK_RESULT:%.*]] = call { i8*, i64, %swift.error* } (i32, i8*, i8*, ...) @llvm.coro.suspend.async.sl_p0i8i64p0s_swift.errorss({{.*}}, %swift.context* {{.*}}, i64 [[NATIVE_ENUM_VAL]], i64 [[NATIVE_INT_VAL]], %T27distributed_actor_accessors7MyActorC* {{.*}})
// CHECK-NEXT: [[TASK_REF:%.*]] = extractvalue { i8*, i64, %swift.error* } [[THUNK_RESULT]], 0
// CHECK-NEXT: {{.*}} = call i8* @__swift_async_resume_project_context(i8* [[TASK_REF]])
// CHECK: [[ENUM_RESULT:%.*]] = extractvalue { i8*, i64, %swift.error* } [[THUNK_RESULT]], 1
// CHECK: {{.*}} = call i1 (i8*, i1, ...) @llvm.coro.end.async({{.*}}, %swift.context* {{.*}}, i64 [[ENUM_RESULT]], %swift.error* {{.*}})

/// ---> Thunk and distributed method for `complex`

// CHECK: define hidden swifttailcc void @"$s27distributed_actor_accessors7MyActorC7complexyAA11LargeStructVSaySiG_AA3ObjCSSSgAFtFTE"

// CHECK: define internal swifttailcc void @"$s27distributed_actor_accessors7MyActorC7complexyAA11LargeStructVSaySiG_AA3ObjCSSSgAFtFTETF"(%T27distributed_actor_accessors11LargeStructV* noalias nocapture [[INDIRECT_RES:%.*]], %swift.context* swiftasync %1, i8* %2, %swift.refcounted* swiftself %3)

/// First, let's check that all of the different argument types here are loaded correctly.

/// -> [Int]

// CHECK: %elt_offset = load i8*, i8** %offset, align 8
// CHECK-NEXT: [[ARR_PTR:%.*]] = bitcast i8* %elt_offset to %TSa*
// CHECK-NEXT: %argval = load %TSa, %TSa* [[ARR_PTR]], align 8
// CHECK: store %TSa %argval, %TSa* %argval.coercion.coerced, align 8
// CHECK-NEXT: [[PTR_TO_NATIVE_ARR:%.*]] = bitcast %TSa* %argval.coercion.coerced to %swift.bridge**
// CHECK-NEXT: [[NATIVE_ARR_VAL:%.*]] = load %swift.bridge*, %swift.bridge** [[PTR_TO_NATIVE_ARR]], align 8
// CHECK: [[ARR_PTR_INT:%.*]] = ptrtoint %TSa* [[ARR_PTR]] to i64
// CHECK-NEXT: [[NEXT_ELT:%.*]] = add i64 [[ARR_PTR_INT]], 8
// CHECK-NEXT: [[OPAQUE_NEXT_ELT:%.*]] = inttoptr i64 [[NEXT_ELT]] to i8*
// CHECK-NEXT: store i8* [[OPAQUE_NEXT_ELT]], i8** %offset, align 8

/// -> Obj

// CHECK-NEXT: %elt_offset1 = load i8*, i8** %offset, align 8
// CHECK-NEXT: [[OBJ_PTR:%.*]] = bitcast i8* %elt_offset1 to %T27distributed_actor_accessors3ObjC**
// CHECK-NEXT: [[NATIVE_OBJ_VAL:%.*]] = load %T27distributed_actor_accessors3ObjC*, %T27distributed_actor_accessors3ObjC** [[OBJ_PTR]], align 8
// CHECK-NEXT: [[OBJ_PTR_INT:%.*]] = ptrtoint %T27distributed_actor_accessors3ObjC** [[OBJ_PTR]] to i64
// CHECK-NEXT: [[NEXT_ELT:%.*]] = add i64 [[OBJ_PTR_INT]], 8
// CHECK-NEXT: [[NEXT_ELT_PTR:%.*]] = inttoptr i64 %18 to i8*
// CHECK-NEXT: store i8* [[NEXT_ELT_PTR]], i8** %offset, align 8

/// -> String?

// CHECK-NEXT: %elt_offset3 = load i8*, i8** %offset, align 8
// CHECK-NEXT: [[OPT_PTR:%.*]] = bitcast i8* %elt_offset3 to %TSSSg*
// CHECK-NEXT: %argval4 = load %TSSSg, %TSSSg* [[OPT_PTR]], align 8
// CHECK: store %TSSSg %argval4, %TSSSg* %argval4.coercion.coerced, align 8
// CHECK-NEXT: [[NATIVE_OPT_PTR:%.*]] = bitcast %TSSSg* %argval4.coercion.coerced to { i64, i64 }*
// CHECK-NEXT: [[NATIVE_OPT_VAL:%.*]] = load { i64, i64 }, { i64, i64 }* [[NATIVE_OPT_PTR]], align 8
// CHECK:      [[NATIVE_OPT_VAL_0:%.*]] = extractvalue { i64, i64 } [[NATIVE_OPT_VAL]], 0
// CHECK-NEXT: [[NATIVE_OPT_VAL_1:%.*]] = extractvalue { i64, i64 } [[NATIVE_OPT_VAL]], 1
// CHECK-NEXT: [[OPT_PTR_INT:%.*]] = ptrtoint %TSSSg* [[OPT_PTR]] to i64
// CHECK-NEXT: [[NEXT_ELT:%.*]] = add i64 [[OPT_PTR_INT]], 16
// CHECK-NEXT: [[NEXT_ELT_PTR:%.*]] = inttoptr i64 [[NEXT_ELT]] to i8*
// CHECK-NEXT: store i8* [[NEXT_ELT_PTR]], i8** %offset, align 8

/// -> LargeStruct (passed indirectly)

// CHECK-NEXT: %elt_offset5 = load i8*, i8** %offset, align 8
// CHECK-NEXT: [[STRUCT_PTR:%.*]] = bitcast i8* %elt_offset5 to %T27distributed_actor_accessors11LargeStructV*
// CHECK-NEXT: [[STRUCT_VAL:%.*]] = load %T27distributed_actor_accessors11LargeStructV, %T27distributed_actor_accessors11LargeStructV* [[STRUCT_PTR]], align 8
// CHECK: store %T27distributed_actor_accessors11LargeStructV [[STRUCT_VAL]], %T27distributed_actor_accessors11LargeStructV* %argval6.coercion.coerced, align 8
// CHECK-NEXT: [[PTR_TO_STRUCT:%.*]] = bitcast %T27distributed_actor_accessors11LargeStructV* %argval6.coercion.coerced to %T27distributed_actor_accessors11LargeStructV**
// CHECK-NEXT: [[NATIVE_STRUCT_VAL:%.*]] = load %T27distributed_actor_accessors11LargeStructV*, %T27distributed_actor_accessors11LargeStructV** [[PTR_TO_STRUCT]], align 8

/// Now let's make sure that distributed thunk call uses the arguments correctly

// CHECK: [[THUNK_RESULT:%.*]] = call { i8*, %swift.error* } (i32, i8*, i8*, ...) @llvm.coro.suspend.async.sl_p0i8p0s_swift.errorss({{.*}}, %T27distributed_actor_accessors11LargeStructV* [[INDIRECT_RES]], %swift.context* {{.*}}, %swift.bridge* [[NATIVE_ARR_VAL]], %T27distributed_actor_accessors3ObjC* [[NATIVE_OBJ_VAL]], i64 [[NATIVE_OPT_VAL_0]], i64 [[NATIVE_OPT_VAL_1]], %T27distributed_actor_accessors11LargeStructV* [[NATIVE_STRUCT_VAL]], %T27distributed_actor_accessors7MyActorC* {{.*}})

/// RESULT is returned indirectly so there is nothing to pass to `end`

// CHECK: {{.*}} = call i1 (i8*, i1, ...) @llvm.coro.end.async({{.*}}, %swift.context* {{.*}}, %swift.error* {{.*}})
