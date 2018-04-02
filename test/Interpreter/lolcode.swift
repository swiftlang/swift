// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

do {
  let z = "DOG"
  _ = #lolcode(
    BOTH SAEM z AN "CAT"
    O RLY?
      YA RLY
        VISIBLE "J00 HAV A CAT"
      NO WAI
        VISIBLE "J00 SUX"
    OIC
  )
  // CHECK: J00 SUX
}

do {
  let z = "CAT"
  _ = #lolcode(
    BOTH SAEM z AN "CAT"
    O RLY?
      YA RLY
        VISIBLE "J00 HAV A CAT"
      NO WAI
        VISIBLE "J00 SUX"
    OIC
  )
  // CHECK: J00 HAV A CAT
}
