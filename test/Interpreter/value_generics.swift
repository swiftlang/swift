// RUN: %target-run-simple-swift(-Xfrontend  -disable-availability-checking) | %FileCheck %s

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// REQUIRES: executable_test

struct A<let N: Int, let M: Int> {
  func foo() {
    print("Lower: \(N), Upper: \(M)")
  }
}

extension A where N == 2 {
  struct B {}
}

extension A where M == 5 {
  struct C {}
}

extension A where N == M {
  struct D {}
}

func getA<let N: Int>() -> A<N, N> {
  A()
}

// CHECK: main.A<123, 321>
print(_typeName(A<123, 321>.self, qualified: true))

// CHECK: (extension in main):main.A<2, 9582>.B
print(_typeName(A<2, 9582>.B.self, qualified: true))

// CHECK: (extension in main):main.A<942735, 5>.C
print(_typeName(A<942735, 5>.C.self, qualified: true))

let x: A<-5, -5> = getA()

// CHECK: main.A<-5, -5>
print(_typeName(type(of: x), qualified: true))

// CHECK: Lower: 1, Upper: 8
A<1, 8>().foo()

// CHECK: Lower: 0, Upper: 8
A<0, 8>().foo()

// CHECK: Lower: -1, Upper: 8
A< -1, 8>().foo()

// CHECK: Lower: -2, Upper: 8
A< -2, 8>().foo()

// CHECK: Lower: -3, Upper: 8
A< -3, 8>().foo()

// CHECK: Lower: -4, Upper: 8
A< -4, 8>().foo()

// CHECK: Lower: -5, Upper: 8
A< -5, 8>().foo()

struct PackAndValue<each T, let N: Int> {
  var count: Int { N }
}

func makePackAndValue<each U>(_: repeat (each U).Type) -> PackAndValue<repeat each U, 3> {
  PackAndValue()
}

func makePackAndValue<each U, let M: Int>(
  _: repeat (each U).Type, array: InlineArray<M, Int>
) -> PackAndValue<repeat each U, M> {
  PackAndValue()
}

func makeMixedPackAndValue<each U>(
  _: repeat (each U).Type
) -> PackAndValue<Int, repeat each U, Bool, 4> {
  PackAndValue()
}

// CHECK: {{^}}3{{$}}
print(makePackAndValue(Int.self, String.self).count)

// CHECK: PackAndValue<Pack{Int, String}, 3>
print(type(of: makePackAndValue(Int.self, String.self)))

// CHECK: PackAndValue<Pack{}, 3>
print(type(of: makePackAndValue()))

// CHECK: {{^}}5{{$}}
print(makePackAndValue(Int.self, array: [1, 2, 3, 4, 5]).count)

// CHECK: PackAndValue<Pack{Int, Double, Bool}, 4>
print(type(of: makeMixedPackAndValue(Double.self)))
