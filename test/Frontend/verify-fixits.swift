// Tests for fix-its on `-verify` mode.

// RUN: not %target-typecheck-verify-swift 2>&1 | %FileCheck %s

func labeledFunc(aa: Int, bb: Int) {}

func testNoneMarkerCheck() {
  // CHECK: [[@LINE+1]]:87: error: A second {{{{}}none}} was found. It may only appear once in an expectation.
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{none}} {{none}}

  // CHECK: [[@LINE+1]]:134: error: {{{{}}none}} must be at the end.
  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{15-18=aa}} {{none}} {{23-26=bb}}
}

func test0Fixits() {
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}}

  // CHECK: [[@LINE+1]]:78: error: expected fix-it not seen
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1-1=a}}

  // CHECK: [[@LINE+1]]:78: error: expected fix-it not seen
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1-1=a}} {{2-2=b}}

  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{none}}

  // CHECK: [[@LINE+1]]:78: error: expected fix-it not seen
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1-1=a}} {{none}}

  // CHECK: [[@LINE+1]]:78: error: expected fix-it not seen
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1-1=a}} {{2-2=b}} {{none}}
}

func test1Fixits() {
  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}}

  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{15-18=aa}}

  // CHECK: [[@LINE+1]]:121: error: expected fix-it not seen; actual fix-it seen: {{{{}}15-18=aa}}
  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{15-18=xx}}

  // CHECK: [[@LINE+1]]:134: error: expected fix-it not seen; actual fix-it seen: {{{{}}15-18=aa}}
  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{15-18=aa}} {{15-18=xx}}

  // CHECK: [[@LINE+1]]:121: error: expected fix-it not seen; actual fix-it seen: {{{{}}15-18=aa}}
  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{15-18=xx}} {{15-18=aa}}

  // CHECK: [[@LINE+1]]:121: error: expected no fix-its; actual fix-it seen: {{{{}}15-18=aa}}
  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{none}}

  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{15-18=aa}} {{none}}

  // CHECK: [[@LINE+1]]:121: error: expected fix-it not seen; actual fix-it seen: {{{{}}15-18=aa}}
  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{15-18=xx}} {{none}}

  // CHECK: [[@LINE+1]]:134: error: expected fix-it not seen; actual fix-it seen: {{{{}}15-18=aa}}
  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{15-18=aa}} {{15-18=xx}} {{none}}

  // CHECK: [[@LINE+1]]:121: error: expected fix-it not seen; actual fix-it seen: {{{{}}15-18=aa}}
  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{15-18=xx}} {{15-18=aa}} {{none}}
}

func test2Fixits() {
  labeledFunc(aax: 0, bbx: 1) // expected-error {{incorrect argument labels in call (have 'aax:bbx:', expected 'aa:bb:')}}

  labeledFunc(aax: 0, bbx: 1) // expected-error {{incorrect argument labels in call (have 'aax:bbx:', expected 'aa:bb:')}} {{15-18=aa}}

  // CHECK: [[@LINE+1]]:124: error: expected fix-it not seen; actual fix-its seen: {{{{}}15-18=aa}} {{{{}}23-26=bb}}
  labeledFunc(aax: 0, bbx: 1) // expected-error {{incorrect argument labels in call (have 'aax:bbx:', expected 'aa:bb:')}} {{15-18=xx}}

  labeledFunc(aax: 0, bbx: 1) // expected-error {{incorrect argument labels in call (have 'aax:bbx:', expected 'aa:bb:')}} {{15-18=aa}} {{23-26=bb}}

  // CHECK: [[@LINE+1]]:137: error: expected fix-it not seen; actual fix-its seen: {{{{}}15-18=aa}} {{{{}}23-26=bb}}
  labeledFunc(aax: 0, bbx: 1) // expected-error {{incorrect argument labels in call (have 'aax:bbx:', expected 'aa:bb:')}} {{15-18=aa}} {{23-26=xx}}

  // CHECK: [[@LINE+1]]:124: error: expected no fix-its; actual fix-its seen: {{{{}}15-18=aa}} {{{{}}23-26=bb}}
  labeledFunc(aax: 0, bbx: 1) // expected-error {{incorrect argument labels in call (have 'aax:bbx:', expected 'aa:bb:')}} {{none}}

  // CHECK: [[@LINE+1]]:137: error: unexpected fix-it seen; actual fix-its seen: {{{{}}15-18=aa}} {{{{}}23-26=bb}}
  labeledFunc(aax: 0, bbx: 1) // expected-error {{incorrect argument labels in call (have 'aax:bbx:', expected 'aa:bb:')}} {{15-18=aa}} {{none}}

  labeledFunc(aax: 0, bbx: 1) // expected-error {{incorrect argument labels in call (have 'aax:bbx:', expected 'aa:bb:')}} {{15-18=aa}} {{23-26=bb}} {{none}}

  // CHECK: [[@LINE+1]]:137: error: expected fix-it not seen; actual fix-its seen: {{{{}}15-18=aa}} {{{{}}23-26=bb}}
  labeledFunc(aax: 0, bbx: 1) // expected-error {{incorrect argument labels in call (have 'aax:bbx:', expected 'aa:bb:')}} {{15-18=aa}} {{23-26=xx}} {{none}}
}
