// RUN: %empty-directory(%t)

// The header dispatches through the vtable of an exposed class, which can reach
// overrides in subclasses that the header does not print.
// RUN: %target-swift-frontend %s -module-name Lib -parse-as-library -O -emit-ir -o %t/Lib.ll \
// RUN:   -emit-clang-header-path %t/Lib.h -clang-header-expose-decls=has-expose-attr -emit-clang-header-min-access internal
// RUN: %FileCheck %s < %t/Lib.ll
// RUN: %check-interop-cxx-header-in-clang(%t/Lib.h)

// With library evolution, the header calls methods of a non-final class
// through their dispatch thunks, which are emitted for internal methods too.
// RUN: %target-swift-frontend %s -module-name Lib -parse-as-library -O -emit-ir -o %t/LibEvo.ll -enable-library-evolution \
// RUN:   -emit-clang-header-path %t/LibEvo.h -clang-header-expose-decls=has-expose-attr -emit-clang-header-min-access internal
// RUN: %FileCheck %s --check-prefix=HEADER-EVO < %t/LibEvo.h
// RUN: %FileCheck %s --check-prefixes=CHECK,EVO < %t/LibEvo.ll
// RUN: %check-interop-cxx-header-in-clang(%t/LibEvo.h)

// Without '-emit-clang-header-min-access internal', the header does not print
// internal methods, so they need no dispatch thunks and their overrides can be
// removed.
// RUN: %target-swift-frontend %s -module-name Lib -parse-as-library -O -emit-ir -o %t/LibPublic.ll -enable-library-evolution \
// RUN:   -emit-clang-header-path %t/LibPublic.h -clang-header-expose-decls=has-expose-attr
// RUN: %FileCheck %s --check-prefix=PUBLIC < %t/LibPublic.ll
// RUN: %FileCheck %s --check-prefix=PUBLIC-NEG < %t/LibPublic.ll

@_expose(Cxx) public class Base {
  public init() {}
  public func publicValue() -> Int { 0 }
  func value() -> Int { 1 }
  var name: Int { 10 }
  private func privateValue() -> Int { 2 }
}

@_expose(Cxx) public final class Derived: Base {
  override func value() -> Int { 3 }
  override var name: Int { 30 }
}

class Hidden: Base {
  override func value() -> Int { 4 }
  override var name: Int { 40 }
}

private class PrivateSub: Base {
  override func value() -> Int { 5 }
}

@_expose(Cxx) public func makeBase() -> Base { Base() }
@_expose(Cxx) public func makeDerived() -> Base { Derived() }
@_expose(Cxx) public func makeHidden() -> Base { Hidden() }
@_expose(Cxx) public func makePrivate() -> Base { PrivateSub() }

// HEADER-EVO-DAG: $s3Lib4BaseC5valueSiyFTj
// HEADER-EVO-DAG: $s3Lib4BaseC4nameSivgTj

// CHECK-DAG: define {{.*}} @"$s3Lib4BaseC5valueSiyF"(
// CHECK-DAG: define {{.*}} @"$s3Lib4BaseC4nameSivg"(
// CHECK-DAG: define {{.*}} @"$s3Lib7DerivedC5valueSiyF"(
// CHECK-DAG: define {{.*}} @"$s3Lib7DerivedC4nameSivg"(
// CHECK-DAG: define {{.*}} @"$s3Lib6HiddenC5valueSiyF"(
// CHECK-DAG: define {{.*}} @"$s3Lib6HiddenC4nameSivg"(
// CHECK-DAG: define {{.*}} @"$s3Lib{{.*}}PrivateSub{{.*}}5valueSiyF"(

// EVO-DAG: define {{.*}} @"$s3Lib4BaseC5valueSiyFTj"(
// EVO-DAG: define {{.*}} @"$s3Lib4BaseC4nameSivgTj"(

// PUBLIC: define {{.*}} @"$s3Lib4BaseC11publicValueSiyFTj"(
// PUBLIC-NEG-NOT: define {{.*}} @"$s3Lib4BaseC5valueSiyFTj"(
// PUBLIC-NEG-NOT: define {{.*}} @"$s3Lib4BaseC4nameSivgTj"(
// PUBLIC-NEG-NOT: define {{.*}}privateValue{{.*}}Tj
// PUBLIC-NEG-NOT: define {{.*}} @"$s3Lib6HiddenC5valueSiyF"(
