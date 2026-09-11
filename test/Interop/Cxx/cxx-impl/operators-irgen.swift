// Verifies that a `@cxx @implementation` of a C++ operator is emitted under
// the mangled symbol of the operator it implements, and that Swift-side uses
// of the imported operators reach the same entry points.

// RUN: %target-swift-emit-ir \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -disable-availability-checking \
// RUN:   -I %S/Inputs \
// RUN:   %s | %FileCheck %s --check-prefixes=CHECK,CHECK-%target-abi

// REQUIRES: swift_feature_CxxImplementation

import Operators


extension Vector {
  // bool Vector::operator==(const Vector &other) const;
  // CHECK-SYSV-LABEL: define{{.*}} i1 @_ZNK6VectoreqERKS_(ptr %0, ptr %1)
  // CHECK-WIN-LABEL: define{{.*}} i1 @"??8Vector@@QEBA_NAEBU0@@Z"(ptr %0, ptr %1)
  @cxx(`operator==`) @implementation
  public func equals(_ other: UnsafePointer<Vector>) -> Bool { return x == other.pointee.x }

  // bool Vector::operator<(const Vector &other) const;
  // CHECK-SYSV-LABEL: define{{.*}} i1 @_ZNK6VectorltERKS_(ptr %0, ptr %1)
  // CHECK-WIN-LABEL: define{{.*}} i1 @"??MVector@@QEBA_NAEBU0@@Z"(ptr %0, ptr %1)
  @cxx(`operator<`) @implementation
  public func less(_ other: UnsafePointer<Vector>) -> Bool { return x < other.pointee.x }

  // Vector Vector::operator+(const Vector &other) const;
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK6VectorplERKS_(ptr %0, ptr %1)
  // CHECK-WIN-LABEL: define{{.*}} @"??HVector@@QEBA?AU0@AEBU0@@Z"(ptr %0, ptr {{[^,]*}}sret
  @cxx(`operator+`) @implementation
  public func plus(_ other: UnsafePointer<Vector>) -> Vector { return Vector(x: x + other.pointee.x) }

  // Vector Vector::operator+(int k) const;
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK6VectorplEi(ptr %0, i32 %1)
  // CHECK-WIN-LABEL: define{{.*}} @"??HVector@@QEBA?AU0@H@Z"(ptr %0, ptr {{[^,]*}}sret
  @cxx(`operator+`) @implementation
  public func plus(_ k: Int32) -> Vector { return Vector(x: x + k) }

  // Vector Vector::operator-() const;
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK6VectorngEv(ptr %0)
  // CHECK-WIN-LABEL: define{{.*}} @"??GVector@@QEBA?AU0@XZ"(ptr %0, ptr {{[^,]*}}sret
  @cxx(`operator-`) @implementation
  public func negated() -> Vector { return Vector(x: -x) }

  // Vector Vector::operator-(const Vector &other) const;
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK6VectormiERKS_(ptr %0, ptr %1)
  // CHECK-WIN-LABEL: define{{.*}} @"??GVector@@QEBA?AU0@AEBU0@@Z"(ptr %0, ptr {{[^,]*}}sret
  @cxx(`operator-`) @implementation
  public func minus(_ other: UnsafePointer<Vector>) -> Vector { return Vector(x: x - other.pointee.x) }

  // Vector &Vector::operator+=(const Vector &other);
  // CHECK-SYSV-LABEL: define{{.*}} ptr @_ZN6VectorpLERKS_(ptr %0, ptr %1)
  // CHECK-WIN-LABEL: define{{.*}} ptr @"??YVector@@QEAAAEAU0@AEBU0@@Z"(ptr %0, ptr %1)
  @cxx(`operator+=`) @implementation
  public mutating func plusEquals(_ other: UnsafePointer<Vector>) -> UnsafeMutablePointer<Vector> {
    x += other.pointee.x
    return withUnsafeMutablePointer(to: &self) { $0 }
  }

  // int Vector::operator[](int i) const;
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK6VectorixEi(ptr %0, i32 %1)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"??AVector@@QEBAHH@Z"(ptr %0, i32 %1)
  @cxx(`operator[]`) @implementation
  public func element(_ i: Int32) -> Int32 { return x + i }

  // int Vector::operator()(int i) const;
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZNK6VectorclEi(ptr %0, i32 %1)
  // CHECK-WIN-LABEL: define{{.*}} i32 @"??RVector@@QEBAHH@Z"(ptr %0, i32 %1)
  @cxx(`operator()`) @implementation
  public func call(_ i: Int32) -> Int32 { return x * i }

  // Vector &Vector::operator++();
  // CHECK-SYSV-LABEL: define{{.*}} ptr @_ZN6VectorppEv(ptr %0)
  // CHECK-WIN-LABEL: define{{.*}} ptr @"??EVector@@QEAAAEAU0@XZ"(ptr %0)
  @cxx(`operator++`) @implementation
  public mutating func increment() -> UnsafeMutablePointer<Vector> {
    x += 1
    return withUnsafeMutablePointer(to: &self) { $0 }
  }

  // Vector Vector::operator++(int);
  // CHECK-SYSV-LABEL: define{{.*}} i32 @_ZN6VectorppEi(ptr %0, i32 %1)
  // CHECK-WIN-LABEL: define{{.*}} @"??EVector@@QEAA?AU0@H@Z"(ptr %0, ptr {{[^,]*}}sret
  @cxx(`operator++`) @implementation
  public mutating func postIncrement(_: Int32) -> Vector {
    let old = self
    x += 1
    return old
  }
}

// bool operator!=(const Vector &a, const Vector &b);
// CHECK-SYSV-LABEL: define{{.*}} i1 @_ZneRK6VectorS1_(ptr %0, ptr %1)
// CHECK-WIN-LABEL: define{{.*}} i1 @"??9@YA_NAEBUVector@@0@Z"(ptr %0, ptr %1)
@cxx @implementation
public func != (a: UnsafePointer<Vector>, b: UnsafePointer<Vector>) -> Bool { return a.pointee.x != b.pointee.x }

// Vector operator*(const Vector &a, int k);
// CHECK-SYSV-LABEL: define{{.*}} i32 @_ZmlRK6Vectori(ptr %0, i32 %1)
// CHECK-WIN-LABEL: define{{.*}} i32 @"??D@YA?AUVector@@AEBU0@H@Z"(ptr %0, i32 %1)
@cxx(`operator*`) @implementation
public func times(_ a: UnsafePointer<Vector>, _ k: Int32) -> Vector { return Vector(x: a.pointee.x * k) }

// bool Outer::operator==(const Point &a, const Point &b);
// CHECK-SYSV-LABEL: define{{.*}} i1 @_ZN5OutereqERKNS_5PointES2_(ptr %0, ptr %1)
// CHECK-WIN-LABEL: define{{.*}} i1 @"??8Outer@@YA_NAEBUPoint@0@0@Z"(ptr %0, ptr %1)
@cxx @implementation
public func == (a: UnsafePointer<Outer.Point>, b: UnsafePointer<Outer.Point>) -> Bool { return a.pointee.v == b.pointee.v }

extension Handle {
  // bool Handle::operator==(const Handle &other) const;
  // CHECK-SYSV-LABEL: define{{.*}} i1 @_ZNK6HandleeqERKS_(ptr %0, ptr %1)
  // CHECK-WIN-LABEL: define{{.*}} i1 @"??8Handle@@QEBA_NAEBU0@@Z"(ptr %0, ptr %1)
  @cxx(`operator==`) @implementation
  public func equals(_ other: Handle) -> Bool { return value == other.value }
}

// bool operator<(const Handle &a, const Handle &b);
// CHECK-SYSV-LABEL: define{{.*}} i1 @_ZltRK6HandleS1_(ptr %0, ptr %1)
// CHECK-WIN-LABEL: define{{.*}} i1 @"??M@YA_NAEBUHandle@@0@Z"(ptr %0, ptr %1)
@cxx @implementation
public func < (a: Handle, b: Handle) -> Bool { return a.value < b.value }

// Swift-side uses of the imported operators call the implemented entry points,
// directly for a free operator and through the synthesized Swift operator
// function for a member operator.
// CHECK-LABEL: define{{.*}} @"$s{{.*}}13callOperators
// CHECK-SYSV-DAG: invoke{{.*}} @_ZneRK6VectorS1_(
// CHECK-SYSV-DAG: invoke{{.*}} @_ZmlRK6Vectori(
// CHECK-SYSV-DAG: invoke{{.*}} @_ZN5OutereqERKNS_5PointES2_(
// CHECK-SYSV-DAG: invoke{{.*}} @_ZltRK6HandleS1_(
// CHECK-SYSV-DAG: invoke{{.*}} @_ZNK6VectorclEi(
// CHECK-SYSV-DAG: invoke{{.*}} @_ZNK6VectoreqERKS_(
// CHECK-SYSV-DAG: invoke{{.*}} @_ZNK6VectorplERKS_(
// CHECK-SYSV-DAG: invoke{{.*}} @_ZNK6VectorplEi(
// CHECK-SYSV-DAG: invoke{{.*}} @_ZNK6VectorngEv(
// CHECK-SYSV-DAG: invoke{{.*}} @_ZN6VectorpLERKS_(
// CHECK-SYSV-DAG: invoke{{.*}} @_ZNK6VectorixEi(
// CHECK-SYSV-DAG: invoke{{.*}} @_ZNK6HandleeqERKS_(
public func callOperators(_ a: inout Vector, _ b: Vector, _ p: Outer.Point, _ h: Handle) -> Int32 {
  var n: Int32 = 0
  if a != b { n += 1 }
  n += (a * 3).x
  if p == p { n += 1 }
  if h < h { n += 1 }
  n += a(2)
  if a == b { n += 1 }
  n += (a + b).x
  n += (a + 2).x
  n += (-a).x
  a += b
  n += a[1]
  if h == h { n += 1 }
  return n
}
