// RUN: not %target-swift-frontend %s -verify -emit-sil -o /dev/null 2>&1 | %FileCheck %s
// RUN: not %target-swift-frontend %s -verify -DFIRST -verify-additional-prefix first- -DFIRST -emit-sil -o /dev/null 2>&1 | %FileCheck -check-prefix=FIRST %s
// RUN: not %target-swift-frontend %s -verify -DSECOND -verify-additional-prefix second- -DSECOND  -emit-sil -o /dev/null 2>&1 | %FileCheck -check-prefix=SECOND %s
// RUN: not %target-swift-frontend %s -verify -DBOTH -verify-additional-prefix first-  -verify-additional-prefix second- -DFIRST -DSECOND -emit-sil -o /dev/null 2>&1 | %FileCheck -check-prefix=BOTH %s

func test() {
  // CHECK-NOT: initialization of immutable value 'normal'
  // FIRST-NOT: initialization of immutable value 'normal'
  // SECOND-NOT: initialization of immutable value 'normal'
  // BOTH-NOT: initialization of immutable value 'normal'
  let normal = 5 // expected-warning {{initialization of immutable value 'normal'}}

  // CHECK: initialization of immutable value 'first'
  // FIRST-NOT: initialization of immutable value 'first'
  // SECOND: initialization of immutable value 'first'
  // BOTH-NOT: initialization of immutable value 'first'
  let first = 5 // expected-first-warning {{initialization of immutable value 'first'}}

  // CHECK: initialization of immutable value 'second'
  // FIRST: initialization of immutable value 'second'
  // SECOND-NOT: initialization of immutable value 'second'
  // BOTH-NOT: initialization of immutable value 'second'
  let second = 5 // expected-second-warning {{initialization of immutable value 'second'}}

  // CHECK: initialization of immutable value 'third'
  // FIRST: initialization of immutable value 'third'
  // SECOND: initialization of immutable value 'third'
  // BOTH: initialization of immutable value 'third'
  let third = 5

  // In this case, BOTH is accepting both first and second... so because we have
  // a first/second warning on the next line, we fail since we consume the
  // warning for the first diagnostic.
  #if BOTH
  // BOTH: expected warning not produced
  let fourth = 5
  // expected-second-warning @-1 {{initialization of immutable value 'fourth'}}
  // expected-first-warning @-2 {{initialization of immutable value 'fourth'}}
  #endif
}
