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
  labeledFunc(0, bb: 1) // expected-error {{missing argument label 'aa:' in call}}

  labeledFunc(0, bb: 1) // expected-error {{missing argument label 'aa:' in call}} {{15-15=aa: }}

  // CHECK: [[@LINE+1]]:84: error: expected fix-it not seen; actual fix-it seen: {{{{}}15-15=aa: }}
  labeledFunc(0, bb: 1) // expected-error {{missing argument label 'aa:' in call}} {{15-15=xx: }}

  // CHECK: [[@LINE+1]]:99: error: expected fix-it not seen; actual fix-it seen: {{{{}}15-15=aa: }}
  labeledFunc(0, bb: 1) // expected-error {{missing argument label 'aa:' in call}} {{15-15=aa: }} {{15-15=xx: }}

  // CHECK: [[@LINE+1]]:84: error: expected fix-it not seen; actual fix-it seen: {{{{}}15-15=aa: }}
  labeledFunc(0, bb: 1) // expected-error {{missing argument label 'aa:' in call}} {{15-15=xx: }} {{15-15=aa: }}

  // CHECK: [[@LINE+1]]:84: error: expected no fix-its; actual fix-it seen: {{{{}}15-15=aa: }}
  labeledFunc(0, bb: 1) // expected-error {{missing argument label 'aa:' in call}} {{none}}

  labeledFunc(0, bb: 1) // expected-error {{missing argument label 'aa:' in call}} {{15-15=aa: }} {{none}}

  // CHECK: [[@LINE+1]]:84: error: expected fix-it not seen; actual fix-it seen: {{{{}}15-15=aa: }}
  labeledFunc(0, bb: 1) // expected-error {{missing argument label 'aa:' in call}} {{15-15=xx: }} {{none}}

  // CHECK: [[@LINE+1]]:99: error: expected fix-it not seen; actual fix-it seen: {{{{}}15-15=aa: }}
  labeledFunc(0, bb: 1) // expected-error {{missing argument label 'aa:' in call}} {{15-15=aa: }} {{15-15=xx: }} {{none}}

  // CHECK: [[@LINE+1]]:84: error: expected fix-it not seen; actual fix-it seen: {{{{}}15-15=aa: }}
  labeledFunc(0, bb: 1) // expected-error {{missing argument label 'aa:' in call}} {{15-15=xx: }} {{15-15=aa: }} {{none}}
}

func test2Fixits() {
  labeledFunc(0, 1) // expected-error {{missing argument labels 'aa:bb:' in call}}

  labeledFunc(0, 1) // expected-error {{missing argument labels 'aa:bb:' in call}} {{15-15=aa: }}

  // CHECK: [[@LINE+1]]:84: error: expected fix-it not seen; actual fix-its seen: {{{{}}15-15=aa: }} {{{{}}18-18=bb: }}
  labeledFunc(0, 1) // expected-error {{missing argument labels 'aa:bb:' in call}} {{15-15=xx: }}

  labeledFunc(0, 1) // expected-error {{missing argument labels 'aa:bb:' in call}} {{15-15=aa: }} {{18-18=bb: }}

  // CHECK: [[@LINE+1]]:99: error: expected fix-it not seen; actual fix-its seen: {{{{}}15-15=aa: }} {{{{}}18-18=bb: }}
  labeledFunc(0, 1) // expected-error {{missing argument labels 'aa:bb:' in call}} {{15-15=aa: }} {{18-18=xx: }}

  // CHECK: [[@LINE+1]]:84: error: expected no fix-its; actual fix-its seen: {{{{}}15-15=aa: }} {{{{}}18-18=bb: }}
  labeledFunc(0, 1) // expected-error {{missing argument labels 'aa:bb:' in call}} {{none}}

  // CHECK: [[@LINE+1]]:99: error: unexpected fix-it seen; actual fix-its seen: {{{{}}15-15=aa: }} {{{{}}18-18=bb: }}
  labeledFunc(0, 1) // expected-error {{missing argument labels 'aa:bb:' in call}} {{15-15=aa: }} {{none}}

  labeledFunc(0, 1) // expected-error {{missing argument labels 'aa:bb:' in call}} {{15-15=aa: }} {{18-18=bb: }} {{none}}

  // CHECK: [[@LINE+1]]:99: error: expected fix-it not seen; actual fix-its seen: {{{{}}15-15=aa: }} {{{{}}18-18=bb: }}
  labeledFunc(0, 1) // expected-error {{missing argument labels 'aa:bb:' in call}} {{15-15=aa: }} {{18-18=xx: }} {{none}}
}
