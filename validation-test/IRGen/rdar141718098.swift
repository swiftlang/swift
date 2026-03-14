// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

struct S<T> {
  let t: T
  func f<each A, each B>(
    _ b: S<(repeat each B)>
  ) -> S<(repeat each A, repeat each B)>
  where T == (repeat each A) {
    S<(repeat each A, repeat each B)>(
      t: (repeat each t, repeat each b.t)
    )
  }
}

func doit() {
  let s1 = S(t: ("", ""))
// CHECK: S<(String, String)>(t: ("", ""))
  print(s1)
  let s2 = s1.f(S(t: (5, 5)))
// CHECK: S<(String, String, Int, Int)>(t: ("", "", 5, 5))
  print(s2)
  let s3 = s2.f(S(t: (13.1, 26.2)))
// CHECK: S<(String, String, Int, Int, Double, Double)>(t: ("", "", 5, 5, 13.1, 26.2))
  print(s3)
  let s4 = s3.f(S(t: (S(t: ""), S(t: ""))))
// CHECK: S<(String, String, Int, Int, Double, Double, S<String>, S<String>)>(t: ("", "", 5, 5, 13.1, 26.2, main.S<Swift.String>(t: ""), main.S<Swift.String>(t: "")))
  print(s4)
}

doit()
