// RUN: %target-swift-frontend -emit-ir -primary-file %s -enable-experimental-feature VariadicGenerics | %FileCheck %s

// Because of -enable-experimental-feature VariadicGenerics
// REQUIRES: asserts

// REQUIRES: PTRSIZE=64

struct G<T...> {
  // CHECK-LABEL: define hidden swiftcc void @"$s22variadic_generic_types1GV6calleeyyF"(i64 %0, %swift.type* %T)
  // CHECK: ret void
  func callee() {}

  // CHECK-LABEL: define hidden swiftcc void @"$s22variadic_generic_types1GV6calleryyF"(i64 %0, %swift.type* %T)
  // CHECK: call swiftcc void @"$s22variadic_generic_types1GV6calleeyyF"(i64 %0, %swift.type* %T)
  // CHECK: ret void
  func caller() {
    callee()
  }

  func makeTuple1() -> (T...).Type {
    return (T...).self
  }

  func makeTuple2() -> ((Array<T>)...).Type {
    return ((Array<T>)...).self
  }
}

func blackHole<T>(_: T) {}

blackHole(G< >.self)
blackHole(G<Int, String>.self)

let g = G<Int, String, Float>()
blackHole(g.makeTuple1())
blackHole(g.makeTuple2())
