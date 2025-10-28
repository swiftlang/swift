// RUN: %target-typecheck-verify-swift

class C {
  var x = 0
}

do {
  let x = C()
  let _ = (0, try x.x)  // expected-warning {{no calls to throwing functions occur within 'try' expression}}
}
