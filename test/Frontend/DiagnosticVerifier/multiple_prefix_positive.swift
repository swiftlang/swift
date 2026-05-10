// RUN: %target-swift-frontend %s -verify -emit-sil -o /dev/null
// RUN: %target-swift-frontend %s -verify -verify-additional-prefix first- -DFIRST -emit-sil -o /dev/null
// RUN: %target-swift-frontend %s -verify -verify-additional-prefix second- -DSECOND  -emit-sil -o /dev/null
// RUN: %target-swift-frontend %s -verify -verify-additional-prefix first-  -verify-additional-prefix second- -DFIRST -DSECOND -emit-sil -o /dev/null

func test() {
  let x = 5 // expected-warning {{}}

  #if FIRST
  let y = 5 // expected-first-warning {{}}
  #endif

  #if SECOND
  let z = 5 // expected-second-warning {{}}
  #endif
}
