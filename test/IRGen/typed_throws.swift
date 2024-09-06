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
