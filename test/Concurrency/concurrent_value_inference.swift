// RUN: %target-typecheck-verify-swift

class C1 { }
final class C2: ConcurrentValue { }

struct S1 {
  var x: Int
  var s: String
  var c: C2
}

enum E1 {
  case base
  indirect case nested(E1)
}

enum E2 {
  case s1(S1)
  case c2(C2)
}

struct GS1<T> { }

struct GS2<T> {
  var storage: T
}

func acceptCV<T: ConcurrentValue>(_: T) { }
// expected-note@-1 3{{where 'T' =}}

func testCV(
  c1: C1, c2: C2, s1: S1, e1: E1, e2: E2, gs1: GS1<Int>, gs2: GS2<Int>
) {
  acceptCV(c1) // expected-error{{'C1' conform to 'ConcurrentValue'}}
  acceptCV(c2)
  acceptCV(s1)
  acceptCV(e1) // expected-error{{'E1' conform to 'ConcurrentValue'}}
  acceptCV(e2)
  acceptCV(gs1)
  acceptCV(gs2) // expected-error{{'GS2<Int>' conform to 'ConcurrentValue'}}
}
