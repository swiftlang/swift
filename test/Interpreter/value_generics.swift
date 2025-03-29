// RUN: %target-run-simple-swift(-Xfrontend  -disable-availability-checking) | %FileCheck %s

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// REQUIRES: executable_test

struct A<let N: Int, let M: Int> {}

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
