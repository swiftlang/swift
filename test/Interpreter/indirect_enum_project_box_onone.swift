// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// Regression test for LoadableByAddress forwarding a project_box address past
// the release of an indirect enum box (#91283).

indirect enum Response<Value> {
  case success(Int, Value)
  case failure(Int)
}

struct Payload {
  let a: String
  let b: String
  let c: String
  let d: String
}

@inline(never)
func unwrap(_ response: Response<Payload>) -> Payload {
  switch response {
  case .success(_, let payload):
    return payload
  case .failure:
    fatalError()
  }
}

// CHECK: PASS
func testit() {
  let payload = unwrap(
    .success(1, Payload(a: "aa", b: "bb", c: "cc", d: "dd")))
  let ok = payload.a == "aa" && payload.b == "bb"
    && payload.c == "cc" && payload.d == "dd"
  print(ok ? "PASS" : "FAIL")
}

testit()
