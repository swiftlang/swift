// RUN: %target-swift-frontend -primary-file %s -emit-ir -disable-availability-checking -runtime-compatibility-version none -target %module-target-future | %FileCheck %s --check-prefix=CHECK-MANGLE

// RUN: %target-swift-frontend -primary-file %s -emit-ir -disable-availability-checking -runtime-compatibility-version 5.8 -disable-concrete-type-metadata-mangled-name-accessors | %FileCheck %s --check-prefix=CHECK-NOMANGLE

// RUN: %target-swift-frontend -primary-file %s -emit-ir  | %FileCheck %s --check-prefix=CHECK

// RUN: %target-swift-frontend -primary-file %s -emit-ir -enable-library-evolution

// RUN: %target-swift-frontend -primary-file %s -emit-ir -O

// XFAIL: CPU=arm64e
// REQUIRES: PTRSIZE=64

public enum MyBigError: Error {
  case epicFail
  case evenBiggerFail
}

// CHECK-MANGLE: @"$s12typed_throws1XVAA1PAAWP" = hidden global [2 x ptr] [ptr @"$s12typed_throws1XVAA1PAAMc", ptr getelementptr inbounds (i8, ptr @"symbolic ySi_____YKc 12typed_throws10MyBigErrorO", {{i32|i64}} 1)]
@available(SwiftStdlib 6.0, *)
struct X: P {
  typealias A = (Int) throws(MyBigError) -> Void
}

func requiresP<T: P>(_: T.Type) { }

@available(SwiftStdlib 6.0, *)
func createsP() {
  requiresP(X.self)
}

// This is for TypeH.method
// CHECK-NOMANGLE: @"$s12typed_throws5TypeHV6methodySSSiAA10MyBigErrorOYKcvpMV" =


// CHECK-LABEL: define {{.*}}hidden swiftcc ptr @"$s12typed_throws13buildMetatypeypXpyF"()
@available(SwiftStdlib 6.0, *)
func buildMetatype() -> Any.Type {
  typealias Fn = (Int) throws(MyBigError) -> Void

  // CHECK-MANGLE: __swift_instantiateConcreteTypeFromMangledName(ptr @"$sySi12typed_throws10MyBigErrorOYKcMD

  // CHECK-NOMANGLE: call swiftcc %swift.metadata_response @"$sySi12typed_throws10MyBigErrorOYKcMa"
  return Fn.self
}

// // CHECK-NOMANGLE: define linkonce_odr hidden swiftcc %swift.metadata_response @"$sySi12typed_throws10MyBigErrorOYKcMa"
// // CHECK-NOMANGLE: @swift_getExtendedFunctionTypeMetadata({{.*}}@"$s12typed_throws10MyBigErrorOMf"

protocol P {
  associatedtype A
}

// The following ensures that we lower
func passthroughCall<T, E>(_ body: () throws(E) -> T) throws(E) -> T {
  try body()
}

func five() -> Int { 5 }

func fiveOrBust() throws -> Int { 5 }

func fiveOrTypedBust() throws(MyBigError) -> Int { throw MyBigError.epicFail }

func reabstractAsNonthrowing() -> Int {
  passthroughCall(five)
}

func reabstractAsThrowing() throws -> Int {
  try passthroughCall(fiveOrBust)
}


func reabstractAsConcreteThrowing() throws -> Int {
  try passthroughCall(fiveOrTypedBust)
}

// CHECK-LABEL: define {{.*}} swiftcc void @"$sSi12typed_throws10MyBigErrorOIgdzo_SiACIegrzr_TR"(ptr noalias nocapture sret(%TSi) %0, ptr %1, ptr %2, ptr swiftself %3, ptr noalias nocapture swifterror dereferenceable(8) %4, ptr %5)
// CHECK: call swiftcc {{i32|i64}} %1
// CHECK: [[CMP:%.*]] = icmp ne ptr {{%.*}}, null
// CHECK: br i1 [[CMP]], label %typed.error.load


struct S : Error { }

func callee() throws (S) {
  throw S()
}

// This used to crash at compile time.
func testit() throws (S) {
  try callee()
}

// Used to crash in abstract pattern creation.
public struct TypeH {
  public var method: (Int) throws(MyBigError) -> String
}

struct SmallError: Error {
  let x: Int
}

@inline(never)
func throwsSmallError() throws(SmallError) -> (Float, Int) {
  throw SmallError(x: 1)
}

// CHECK: define hidden swiftcc i64 @"$s12typed_throws17catchesSmallErrorSiyF"()
// CHECK:   [[RES:%.*]] = call swiftcc { float, i64 } @"$s12typed_throws0B10SmallErrorSf_SityAA0cD0VYKF"(ptr swiftself undef, ptr noalias nocapture swifterror dereferenceable(8) %swifterror)
// CHECK:   [[R0:%.*]] = extractvalue { float, i64 } [[RES]], 0
// CHECK:   [[R1:%.*]] = extractvalue { float, i64 } [[RES]], 1
// CHECK:   phi i64 [ [[R1]], %typed.error.load ]
// CHECK: }
func catchesSmallError() -> Int {
  do {
    return try throwsSmallError().1
  } catch {
    return error.x
  }
}

struct MyError: Error {
  let x: AnyObject
}

// CHECK: define hidden swiftcc { float, i64, float } @"$s12typed_throws8mayThrow1x1ySf_s5Int32VSftSb_yXltAA7MyErrorVYKF"
// CHECK:   [[CONVERTED:%.*]] = ptrtoint ptr {{%.*}} to i64
// CHECK:   insertvalue { float, i64, float } undef, i64 [[CONVERTED]], 1
// CHECK: }
@inline(never)
func mayThrow(x: Bool, y: AnyObject) throws(MyError) -> (Float, Int32, Float) {
  guard x else {
    throw MyError(x: y)
  }
  return (3.0, 4, 5.0)
}

// CHECK: define hidden swiftcc { i64, i64 } @"$s12typed_throws25directErrorMergePtrAndInt1x1yyXl_SitSb_yXltAA05SmallD0VYKF"
// CHECK:   [[RES:%.*]] = call swiftcc { i64, i64 } @"$s12typed_throws25directErrorMergePtrAndInt1x1yyXl_SitSb_yXltAA05SmallD0VYKF"
// CHECK:   [[R0:%.*]] = extractvalue { i64, i64 } [[RES]], 0
// CHECK:   inttoptr i64 [[R0]] to ptr
// CHECK: }
func directErrorMergePtrAndInt(x: Bool, y: AnyObject) throws(SmallError) -> (AnyObject, Int) {
  guard x else {
    throw SmallError(x: 1)
  }

  return try directErrorMergePtrAndInt(x: !x, y: y)
}

// This used to crash at compile time, because it was trying to use a direct
// error return in combination with an indirect result, which is illegal.
func genericThrows<T>(x: Bool, y: T) throws(SmallError) -> T {
  guard x else {
    throw SmallError(x: 1)
  }

  return y
}

func throwsGeneric<T: Error>(x: Bool, y: T) throws(T) -> Int {
  guard x else {
    throw y
  }

  return 32
}

@available(SwiftStdlib 6.0, *)
func mayThrowAsync(x: Bool, y: AnyObject) async throws(MyError) -> (Float, Int32, Float) {
  guard x else {
    throw MyError(x: y)
  }
  return (3.0, 4, 5.0)
}

@available(SwiftStdlib 6.0, *)
func genericThrowsAsync<T>(x: Bool, y: T) async throws(SmallError) -> T {
  guard x else {
    throw SmallError(x: 1)
  }

  return y
}

@available(SwiftStdlib 6.0, *)
func throwsGenericAsync<T: Error>(x: Bool, y: T) async throws(T) -> Int {
  guard x else {
    throw y
  }

  return 32
}

enum TinyError: Error {
  case a
}

@available(SwiftStdlib 6.0, *)
func mayThrowAsyncTiny(x: Bool) async throws(TinyError) -> Bool {
  guard x else {
    throw .a
  }
  return false
}

@available(SwiftStdlib 6.0, *)
func callsMayThrowAsyncTiny(x: Bool) async {
  _ = try! await mayThrowAsyncTiny(x: x)
}

struct EmptyError: Error {}

@available(SwiftStdlib 6.0, *)
func mayThrowEmptyErrorAsync(x: Bool) async throws(EmptyError) -> String? {
  guard x else {
    throw EmptyError()
  }

  return ""
}


enum SP: Error {
    case a
    case b(Int32)
}

protocol Proto {
  // This used to crash.
  static func f() throws(SP) -> Self

  // This used to crash.
  static func f2() throws(SP) -> Int64
}

@inline(never)
@available(SwiftStdlib 6.0, *)
public func passthroughAsync<T, E: Error>(f: () async throws(E) -> T) async throws(E) -> T {
  try await f()
}

@available(SwiftStdlib 6.0, *)
public func reabstractAsyncVoidThrowsNever() async {
  await passthroughAsync {
    ()
  }
}

// Used to crash with null GenericSignature -- https://github.com/swiftlang/swift/issues/77297
struct LoadableGeneric<E>: Error {}

func throwsLoadableGeneric<E>(_: E) throws(LoadableGeneric<E>) {}

@inline(never)
func throwError() throws(SmallError) -> Never {
  throw SmallError(x: 1)
}

func conditionallyCallsThrowError(b: Bool) throws(SmallError) -> Int {
  if b {
    try throwError()
  } else {
    return 0
  }
}

func passthroughFixedErrorCall<T>(_ body: () throws(TinyError) -> T) throws(TinyError) -> T {
  try body()
}

func passthroughFixedErrorAsync<T>(_ body: () async throws(TinyError) -> T) async throws(TinyError) -> T {
  try await body()
}

func callClosureSync<T>(t: T) {
  _ = try! passthroughFixedErrorCall { () throws(TinyError) -> T in
    return t
  }
}

func callClosureAsync<T>(t: T) async {
  _ = try! await passthroughFixedErrorAsync { () async throws(TinyError) -> T in
    return t
  }
}

enum LargeError: Error {
  case x
  case y(Int64, Int64, Int64, Int64, Int64)
}

// Used to crash the compiler because
func callClosureAsyncIndirectError(f: () async throws(LargeError) -> Int) async throws(LargeError) -> Int {
  return try await f()
}

protocol AsyncGenProto<A> {
  associatedtype A
  func fn(arg: Int) async throws(SmallError) -> A
}

// CHECK: define internal swifttailcc void @"$s12typed_throws23callAsyncIndirectResult1p1xxAA0D8GenProto_px1ARts_XP_SitYaAA10SmallErrorVYKlFTY0_"(ptr swiftasync %0)
// CHECK:   musttail call swifttailcc void {{%.*}}(ptr noalias {{%.*}}, ptr swiftasync {{%.*}}, i64 {{%.*}}, ptr noalias swiftself {{%.*}}, ptr %swifterror, ptr {{%.*}}, ptr {{%.*}})
// CHECK:   ret void
// CHECK: }
@inline(never)
func callAsyncIndirectResult<A>(p: any AsyncGenProto<A>, x: Int) async throws(SmallError) -> A {
  return try await p.fn(arg: x)
}

@inline(never)
func smallResultLargerError() throws(SmallError) -> Int8? {
  return 10
}

// CHECK:  [[COERCED:%.*]] = alloca { i16 }, align 2
// CHECK:  [[RES:%.*]] = call swiftcc i64 @"$s12typed_throws22smallResultLargerErrors4Int8VSgyAA05SmallF0VYKF"(ptr swiftself undef, ptr noalias nocapture swifterror dereferenceable(8) %swifterror)
// CHECK:  [[TRUNC:%.*]] = trunc i64 [[RES]] to i16
// CHECK:  [[COERCED_PTR:%.*]] = getelementptr inbounds { i16 }, ptr [[COERCED]], i32 0, i32 0
// CHECK:  store i16 [[TRUNC]], ptr [[COERCED_PTR]], align 2
func callSmallResultLargerError() {
  let res = try! smallResultLargerError()
  precondition(res! == 10)
}

enum UInt8OptSingletonError: Error {
  case a(Int8?)
}

@inline(never)
func smallErrorLargerResult() throws(UInt8OptSingletonError) -> Int {
  throw .a(10)
}

// CHECK:  [[COERCED:%.*]] = alloca { i16 }, align 2
// CHECK:  [[RES:%.*]] = call swiftcc i64 @"$s12typed_throws22smallErrorLargerResultSiyAA017UInt8OptSingletonD0OYKF"(ptr swiftself undef, ptr noalias nocapture swifterror dereferenceable(8) %swifterror)
// CHECK:  [[TRUNC:%.*]] = trunc i64 [[RES]] to i16
// CHECK:  [[COERCED_PTR:%.*]] = getelementptr inbounds { i16 }, ptr [[COERCED]], i32 0, i32 0
// CHECK:  store i16 [[TRUNC]], ptr [[COERCED_PTR]], align 2
func callSmallErrorLargerResult() {
  do {
    _ = try smallErrorLargerResult()
  } catch {
    switch error {
      case .a(let x):
        precondition(x! == 10)
    }
  }
}

struct SomeStruct {
    let x: Int
    let y: UInt32
    let z: UInt32
}

func someFunc() async throws(SmallError) -> SomeStruct {
    SomeStruct(x: 42, y: 23, z: 25)
}

// Used to crash the compiler -- https://github.com/swiftlang/swift/issues/80732
protocol PAssoc<T>: AnyObject {
    associatedtype T
    func foo() async throws(SmallError) -> (any PAssoc<T>)
}

class MyProtocolImpl<T>: PAssoc {
    func foo() async throws(SmallError) -> (any PAssoc<T>) {
        fatalError()
    }
}
