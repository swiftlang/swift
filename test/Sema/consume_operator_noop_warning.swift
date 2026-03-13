// RUN: %target-swift-emit-sil %s -verify -sil-verify-all

struct Point {
  let x: Float
  let y: Float
}

struct ConditionallyBC<T> {
  var t: T
}
extension ConditionallyBC: BitwiseCopyable where T: BitwiseCopyable {}

func test<T, BCG: BitwiseCopyable>(_ t: T, // expected-error {{'t' is borrowed and cannot be consumed}}
                                   _ bcg: BCG, // expected-error {{'bcg' is borrowed and cannot be consumed}}
                                   _ cbcg_generic: ConditionallyBC<BCG>, // expected-error {{'cbcg_generic' is borrowed and cannot be consumed}}
                                   _ maybeBCG: BCG?, // expected-error {{'maybeBCG' is borrowed and cannot be consumed}}
                                   _ maybeT: T?, // expected-error {{'maybeT' is borrowed and cannot be consumed}}
                                   _ anyBC: any BitwiseCopyable, // expected-error {{'anyBC' is borrowed and cannot be consumed}}
                                   _ x: Int,
                                   _ point: Point,
                                   _ cbcg_concrete: ConditionallyBC<Int>,
                                   _ maybeFloat: Float?) {
  _ = consume t  // expected-note {{consumed here}}
  _ = consume bcg // expected-note {{consumed here}}
  _ = consume cbcg_generic // expected-note {{consumed here}}
  _ = consume maybeBCG // expected-note {{consumed here}}
  _ = consume maybeT // expected-note {{consumed here}}
  _ = consume anyBC // expected-note {{consumed here}}

  _ = consume x // expected-warning {{'consume' applied to bitwise-copyable type 'Int' has no effect}}{{7-15=}}
  _ = consume point // expected-warning {{'consume' applied to bitwise-copyable type 'Point' has no effect}}{{7-15=}}
  _ = consume  cbcg_concrete // expected-warning {{'consume' applied to bitwise-copyable type 'ConditionallyBC<Int>' has no effect}}{{7-16=}}
   _ = consume maybeFloat // expected-warning {{'consume' applied to bitwise-copyable type 'Float?' has no effect}}{{8-16=}}
}

func proofOfUseAfterConsume() -> Int {
  let someInt = 10
  let y = consume someInt // expected-warning {{'consume' applied to bitwise-copyable type 'Int' has no effect}}
  print(y)
  return someInt  // undiagnosed use-after-consume
}

func moreProofs(_ share: __shared Int,
                _ own: __owned Int,
                _ snd: sending Int, // expected-error {{'snd' used after consume}}
                _ ino: inout Int, // expected-error {{'ino' used after consume}}
                _ brw: borrowing Int, // expected-error {{'brw' is borrowed and cannot be consumed}}
                _ csm: consuming Int // expected-error {{'csm' consumed more than once}}
               ) {
  _ = consume share // expected-warning {{'consume' applied to bitwise-copyable type 'Int' has no effect}}
  _ = consume own // expected-warning {{'consume' applied to bitwise-copyable type 'Int' has no effect}}
  let _ = (share, own)

   _ = consume ino // expected-note {{consumed}}
   _ = consume brw // expected-note {{consumed}}
   _ = consume csm // expected-note {{consumed}}
   _ = consume csm // expected-note {{consumed}}
   _ = consume snd // expected-note {{consumed}}
   _ = snd // expected-note {{used}}
} // expected-note {{used here}}
