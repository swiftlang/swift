// RUN: %empty-directory(%t)

// The header only prints exposed declarations: keep their members.
// RUN: %target-swift-frontend %s -module-name Lib -parse-as-library -O -emit-ir -o %t/Lib.ll \
// RUN:   -emit-clang-header-path %t/Lib.h -clang-header-expose-decls=has-expose-attr -emit-clang-header-min-access internal
// RUN: %FileCheck %s --check-prefix=HEADER < %t/Lib.h
// RUN: %FileCheck %s --check-prefix=HEADER-NEG < %t/Lib.h
// RUN: %FileCheck %s --check-prefixes=CHECK,EXPOSED < %t/Lib.ll
// RUN: %FileCheck %s --check-prefixes=NEG,NEG-EXPOSED < %t/Lib.ll
// RUN: %check-interop-cxx-header-in-clang(%t/Lib.h)

// With C++ interop and no '-clang-header-expose-decls', the header prints
// every internal declaration, exposed or not.
// RUN: %target-swift-frontend %s -module-name Lib -parse-as-library -O -emit-ir -o %t/LibAll.ll \
// RUN:   -emit-clang-header-path %t/LibAll.h -cxx-interoperability-mode=default -emit-clang-header-min-access internal
// RUN: %FileCheck %s --check-prefix=HEADER-ALL < %t/LibAll.h
// RUN: %FileCheck %s --check-prefixes=CHECK,ALL < %t/LibAll.ll
// RUN: %FileCheck %s --check-prefix=NEG < %t/LibAll.ll

// Without '-emit-clang-header-min-access internal', the header does not print
// internal declarations, so the optimizer can remove them.
// RUN: %target-swift-frontend %s -module-name Lib -parse-as-library -O -emit-ir -o %t/LibPublic.ll \
// RUN:   -emit-clang-header-path %t/LibPublic.h -clang-header-expose-decls=has-expose-attr
// RUN: %FileCheck %s --check-prefix=PUBLIC < %t/LibPublic.ll
// RUN: %FileCheck %s --check-prefix=PUBLIC-NEG < %t/LibPublic.ll

@_expose(Cxx) public final class Counter {
  var count: Int = 0
  init() {}
  func increment() { count += 1 }
  func value() -> Int { count }
  var observed: Int = 0 { didSet { count += 1 } }
  lazy var cached: Int = 7
  func asyncMethod() async {}
  private func privateHelper() {}
  @_expose(!Cxx) func notExposed() {}
}

@_expose(Cxx) struct Point {
  var x: Int
  var y: Int
  func sum() -> Int { x + y }
  static var origin = 0
}

extension Point {
  func extensionMethod() -> Int { 42 }
}

@_expose(Cxx) enum Direction: Hashable {
  case up, down
  func flipped() -> Direction { self == .up ? .down : .up }
}

@_expose(Cxx) struct Wrapper<T> {
  var value: T
  func get() -> T { value }
}

@_expose(Cxx) func makeCounter() -> Counter { Counter() }

struct NotExposed {
  var z: Int
  func hidden() -> Int { z }
}

enum Raw: Int {
  case one = 1, two
}

// HEADER-DAG: SWIFT_INLINE_THUNK swift::Int getCount() noexcept
// HEADER-DAG: SWIFT_INLINE_THUNK void increment() noexcept
// HEADER-DAG: SWIFT_INLINE_THUNK swift::Int value() noexcept
// HEADER-DAG: SWIFT_INLINE_THUNK swift::Int getX() const noexcept
// HEADER-DAG: SWIFT_INLINE_THUNK swift::Int sum() const noexcept
// HEADER-DAG: SWIFT_INLINE_THUNK swift::Int extensionMethod() const noexcept
// HEADER-DAG: static SWIFT_INLINE_THUNK swift::Int getOrigin() noexcept
// HEADER-DAG: SWIFT_INLINE_THUNK Direction flipped() const noexcept
// HEADER-DAG: SWIFT_INLINE_THUNK swift::Int getHashValue() const noexcept
// HEADER-DAG: SWIFT_INLINE_THUNK void setObserved(swift::Int value) noexcept
// HEADER-DAG: SWIFT_INLINE_THUNK void setCached(swift::Int value) noexcept
// HEADER-DAG: static SWIFT_INLINE_THUNK void setOrigin(swift::Int value) noexcept
// HEADER-DAG: SWIFT_INLINE_THUNK T_0_0 get() const

// HEADER-NEG-NOT: asyncMethod
// HEADER-NEG-NOT: privateHelper
// HEADER-NEG-NOT: notExposed
// HEADER-NEG-NOT: hidden()

// HEADER-ALL-DAG: SWIFT_INLINE_THUNK swift::Int hidden() const noexcept
// HEADER-ALL-DAG: SWIFT_INLINE_THUNK swift::Int getRawValue() const noexcept

// CHECK-DAG: define {{.*}} @"$s3Lib7CounterC9incrementyyF"(
// CHECK-DAG: define {{.*}} @"$s3Lib7CounterC5valueSiyF"(
// CHECK-DAG: define {{.*}} @"$s3Lib7CounterC5countSivg"(
// CHECK-DAG: define {{.*}} @"$s3Lib7CounterC5countSivs"(
// CHECK-DAG: define {{.*}} @"$s3Lib7CounterCACycfC"(
// CHECK-DAG: define {{.*}} @"$s3Lib7CounterC8observedSivs"(
// CHECK-DAG: define {{.*}} @"$s3Lib7CounterC6cachedSivs"(
// CHECK-DAG: define {{.*}} @"$s3Lib5PointV3sumSiyF"(
// CHECK-DAG: define {{.*}} @"$s3Lib5PointV15extensionMethodSiyF"(
// CHECK-DAG: define {{.*}} @"$s3Lib5PointV1xSivg"(
// CHECK-DAG: define {{.*}} @"$s3Lib5PointV1xSivs"(
// CHECK-DAG: define {{.*}} @"$s3Lib5PointV1x1yACSi_SitcfC"(
// CHECK-DAG: define {{.*}} @"$s3Lib5PointV6originSivgZ"(
// CHECK-DAG: define {{.*}} @"$s3Lib5PointV6originSivsZ"(
// CHECK-DAG: define {{.*}} @"$s3Lib5PointVMa"(
// CHECK-DAG: define {{.*}} @"$s3Lib9DirectionO7flippedACyF"(
// CHECK-DAG: define {{.*}} @"$s3Lib9DirectionO9hashValueSivg"(
// CHECK-DAG: define {{.*}} @"$s3Lib9DirectionOMa"(
// CHECK-DAG: define {{.*}} @"$s3Lib7WrapperV3getxyF"(
// CHECK-DAG: define {{.*}} @"$s3Lib7WrapperVMa"(
// CHECK-DAG: define {{.*}} @"$s3Lib11makeCounterAA0C0CyF"(

// ALL-DAG: define {{.*}} @"$s3Lib10NotExposedV6hiddenSiyF"(
// ALL-DAG: define {{.*}} @"$s3Lib10NotExposedVMa"(
// ALL-DAG: define {{.*}} @"$s3Lib3RawO8rawValueACSgSi_tcfC"(
// ALL-DAG: define {{.*}} @"$s3Lib3RawO8rawValueSivg"(

// NEG-NOT: define {{.*}}asyncMethod
// NEG-NOT: define {{.*}}privateHelper
// NEG-NOT: define {{.*}}notExposed
// NEG-NOT: define {{.*}} @"$s3Lib7CounterC5countSivM"(
// NEG-EXPOSED-NOT: define {{.*}}NotExposed
// NEG-EXPOSED-NOT: define {{.*}}3RawO

// The directly exposed function is kept regardless.
// PUBLIC: define {{.*}} @"$s3Lib11makeCounterAA0C0CyF"(
// PUBLIC-NEG-NOT: define {{.*}}increment
// PUBLIC-NEG-NOT: define {{.*}}extensionMethod
// PUBLIC-NEG-NOT: define {{.*}}NotExposed
// PUBLIC-NEG-NOT: define {{.*}} @"$s3Lib5PointV1xSivg"(
// PUBLIC-NEG-NOT: define {{.*}} @"$s3Lib5PointVMa"(
