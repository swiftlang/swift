// Tests for fix-its on `-verify` mode.

// RUN: not %target-typecheck-verify-swift 2>&1 | %FileCheck %s

func labeledFunc(aa: Int, bb: Int) {}

func testNoneMarkerCheck() {
  // CHECK: [[@LINE+1]]:87: error: A second {{\{\{}}none}} was found. It may only appear once in an expectation.
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{none}} {{none}}

  // CHECK: [[@LINE+1]]:134: error: {{\{\{}}none}} must be at the end.
  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{15-18=aa}} {{none}} {{23-26=bb}}
}

func test0Fixits() {
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}}

  // CHECK: [[@LINE+1]]:80: error: expected fix-it verification within braces; example: '1-2=text' or 'none'
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{}}

  // CHECK: [[@LINE+1]]:81: error: expected line offset after leading '+' or '-' in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{+}}

  // CHECK: [[@LINE+1]]:81: error: expected line offset after leading '+' or '-' in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{-}}

  // CHECK: [[@LINE+1]]:81: error: expected '-' range separator in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1}}

  // CHECK: [[@LINE+1]]:82: error: expected colon-separated column number after line offset in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{-1}}

  // CHECK: [[@LINE+1]]:82: error: expected colon-separated column number after line offset in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{+1}}

  // CHECK: [[@LINE+1]]:82: error: expected column number after ':' in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1:}}

  // CHECK: [[@LINE+1]]:83: error: expected column number after ':' in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{+1:}}

  // CHECK: [[@LINE+1]]:83: error: expected '-' range separator in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1:1}}

  // CHECK: [[@LINE+1]]:84: error: expected '-' range separator in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{-1:1}}

  // CHECK: [[@LINE+1]]:83: error: expected column number after ':' in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{+1:-1}}

  // CHECK: [[@LINE+1]]:82: error: expected line or column number in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1-}}

  // CHECK: [[@LINE+1]]:84: error: expected line or column number in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1:1-}}

  // CHECK: [[@LINE+1]]:85: error: expected line or column number in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{-1:1-}}

  // CHECK: [[@LINE+1]]:83: error: expected line offset after leading '+' or '-' in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1--}}

  // CHECK: [[@LINE+1]]:83: error: expected '=' after range in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1-1}}

  // CHECK: [[@LINE+1]]:85: error: expected '=' after range in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1:1-1}}

  // CHECK: [[@LINE+1]]:86: error: expected '=' after range in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{+1:1-1}}

  // CHECK: [[@LINE+1]]:83: error: expected line offset after leading '+' or '-' in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1--:}}

  // CHECK: [[@LINE+1]]:84: error: expected column number after ':' in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1-1:}}

  // CHECK: [[@LINE+1]]:86: error: expected column number after ':' in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1:1-1:}}

  // CHECK: [[@LINE+1]]:87: error: expected column number after ':' in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{+1:1-1:}}

  // CHECK: [[@LINE+1]]:85: error: expected '=' after range in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1-1:1}}

  // CHECK: [[@LINE+1]]:86: error: expected '=' after range in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1-+1:1}}

  // CHECK: [[@LINE+1]]:87: error: expected '=' after range in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1:1-1:1}}

  // CHECK: [[@LINE+1]]:89: error: expected '=' after range in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{+1:1--1:1}}

  // CHECK: [[@LINE+1]]:78: error: expected fix-it not seen
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1-1=}}

  // CHECK: [[@LINE+1]]:78: error: expected fix-it not seen
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1-1=a}}

  // CHECK: [[@LINE+1]]:80: error: expected line or column number in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{x-1=a}}

  // CHECK: [[@LINE+1]]:82: error: expected line or column number in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1-x=a}}

  // CHECK: [[@LINE+1]]:82: error: expected column number after ':' in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1:x-1=a}}

  // CHECK: [[@LINE+1]]:80: error: expected line or column number in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{x:1-1=a}}

  // CHECK: [[@LINE+1]]:81: error: expected line offset after leading '+' or '-' in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{+x:1-1=a}}

  // CHECK: [[@LINE+1]]:84: error: expected column number after ':' in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1-1:x=a}}

  // CHECK: [[@LINE+1]]:82: error: expected line or column number in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1-x:1=a}}

  // CHECK: [[@LINE+1]]:83: error: expected line offset after leading '+' or '-' in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1-+x:1=a}}

  // CHECK: [[@LINE+1]]:82: error: expected column number after ':' in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1:x-1:x=a}}

  // CHECK: [[@LINE+1]]:80: error: expected line or column number in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{x:1-1:x=a}}

  // CHECK: [[@LINE+1]]:81: error: expected line offset after leading '+' or '-' in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{+x:1-1:x=a}}

  // CHECK: [[@LINE+1]]:82: error: expected column number after ':' in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1:x-x:1=a}}

  // CHECK: [[@LINE+1]]:82: error: expected column number after ':' in fix-it verification
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1:x--x:1=a}}

  // CHECK: [[@LINE+1]]:78: error: expected fix-it not seen
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1:1-1:1=a}}

  // CHECK: [[@LINE+1]]:78: error: expected fix-it not seen
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1-1:1=a}}

  // CHECK: [[@LINE+1]]:78: error: expected fix-it not seen
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1:1-1=a}}

  // CHECK: [[@LINE+1]]:78: error: expected fix-it not seen
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{-1:1-1=a}}

  // CHECK: [[@LINE+1]]:78: error: expected fix-it not seen
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1-+1:1=a}}

  // CHECK: [[@LINE+1]]:78: error: expected fix-it not seen
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{+1:1-+1:1=a}}

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

  // CHECK: [[@LINE+1]]:121: error: expected fix-it not seen; actual fix-it seen: {{\{\{}}15-18=aa}}
  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{15-18=xx}}

  // CHECK: [[@LINE+1]]:134: error: expected fix-it not seen; actual fix-it seen: {{\{\{}}15-18=aa}}
  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{15-18=aa}} {{15-18=xx}}

  // CHECK: [[@LINE+1]]:121: error: expected fix-it not seen; actual fix-it seen: {{\{\{}}15-18=aa}}
  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{15-18=xx}} {{15-18=aa}}

  // CHECK: [[@LINE+1]]:121: error: expected no fix-its; actual fix-it seen: {{\{\{}}15-18=aa}}
  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{none}}

  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{15-18=aa}} {{none}}

  // CHECK: [[@LINE+1]]:121: error: expected fix-it not seen; actual fix-it seen: {{\{\{}}15-18=aa}}
  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{15-18=xx}} {{none}}

  // CHECK: [[@LINE+1]]:134: error: expected fix-it not seen; actual fix-it seen: {{\{\{}}15-18=aa}}
  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{15-18=aa}} {{15-18=xx}} {{none}}

  // CHECK: [[@LINE+1]]:121: error: expected fix-it not seen; actual fix-it seen: {{\{\{}}15-18=aa}}
  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{15-18=xx}} {{15-18=aa}} {{none}}

  // CHECK-NOT: [[@LINE+1]]:{{[0-9]+}}: error:
  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{200:15-200:18=aa}}
  // CHECK-NOT: [[@LINE+1]]:{{[0-9]+}}: error:
  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{202:15-18=aa}}
  // CHECK-NOT: [[@LINE+1]]:{{[0-9]+}}: error:
  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{15-204:18=aa}}
  // CHECK-NOT: [[@LINE+1]]:{{[0-9]+}}: error:
  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{-0:15-+0:18=aa}}
  // CHECK-NOT: [[@LINE+1]]:{{[0-9]+}}: error:
  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{15--0:18=aa}}
  // CHECK-NOT: [[@LINE+1]]:{{[0-9]+}}: error:
  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {+0:15-210:18=aa}}
  // CHECK-NOT: [[@LINE+1]]:{{[0-9]+}}: error:
  labeledFunc(aa: 0, // expected-error {{incorrect argument label in call (have 'aa:bbx:', expected 'aa:bb:')}} {{+1:15-+1:18=bb}}
              bbx: 1)
  // CHECK-NOT: [[@LINE+1]]:{{[0-9]+}}: error:
  labeledFunc(aa: 0, // expected-error {{incorrect argument label in call (have 'aa:bbx:', expected 'aa:bb:')}} {{216:15-+1:18=bb}}
              bbx: 1)

  // CHECK: [[@LINE+1]]:121: error: expected fix-it not seen; actual fix-it seen: {{\{\{}}15-18=aa}}
  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{61:15-18=aa}}
  // CHECK: [[@LINE+1]]:121: error: expected fix-it not seen; actual fix-it seen: {{\{\{}}15-18=aa}}
  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{-1:15-18=aa}}
  // CHECK: [[@LINE+1]]:121: error: expected fix-it not seen; actual fix-it seen: {{\{\{}}15-18=aa}}
  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{+0:15--1:18=aa}}
  // CHECK: [[@LINE+1]]:121: error: expected fix-it not seen; actual fix-it seen: {{\{\{}}15-18=aa}}
  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{61:15-+1:18=aa}}
}

func unlabeledFunc(_ aa: Int) {}

func testDefaultedLineNumbers() {
  // Fix-it end line defaults to first line.
  // CHECK-NOT: [[@LINE+1]]:{{[0-9]+}}: error:
  unlabeledFunc(aa: // expected-error {{extraneous argument label 'aa:' in call}} {{+0:17-+1:5=}}
    1)
  // CHECK: [[@LINE+1]]:83: error: expected fix-it not seen; actual fix-it seen: {{\{\{}}17-[[@LINE+2]]:5=}}
  unlabeledFunc(aa: // expected-error {{extraneous argument label 'aa:' in call}} {{+0:17-5=}}
    1)

  // Fix-it start line defaults to diagnostic line.
  // CHECK-NOT: [[@LINE+1]]:{{[0-9]+}}: error:
  labeledFunc(aa: 0, // expected-error {{incorrect argument label in call (have 'aa:bbx:', expected 'aa:bb:')}} {{+1:15-+1:18=bb}}
              bbx: 1)
  // CHECK: [[@LINE+1]]:113: error: expected fix-it not seen; actual fix-it seen: {{\{\{}}[[@LINE+2]]:15-18=bb}}
  labeledFunc(aa: 0, // expected-error {{incorrect argument label in call (have 'aa:bbx:', expected 'aa:bb:')}} {{15-+1:18=bb}}
              bbx: 1)
}

func test2Fixits() {
  labeledFunc(aax: 0, bbx: 1) // expected-error {{incorrect argument labels in call (have 'aax:bbx:', expected 'aa:bb:')}}

  labeledFunc(aax: 0, bbx: 1) // expected-error {{incorrect argument labels in call (have 'aax:bbx:', expected 'aa:bb:')}} {{15-18=aa}}

  // CHECK: [[@LINE+1]]:124: error: expected fix-it not seen; actual fix-its seen: {{\{\{}}15-18=aa}} {{\{\{}}23-26=bb}}
  labeledFunc(aax: 0, bbx: 1) // expected-error {{incorrect argument labels in call (have 'aax:bbx:', expected 'aa:bb:')}} {{15-18=xx}}

  labeledFunc(aax: 0, bbx: 1) // expected-error {{incorrect argument labels in call (have 'aax:bbx:', expected 'aa:bb:')}} {{15-18=aa}} {{23-26=bb}}

  // CHECK: [[@LINE+1]]:137: error: expected fix-it not seen; actual fix-its seen: {{\{\{}}15-18=aa}} {{\{\{}}23-26=bb}}
  labeledFunc(aax: 0, bbx: 1) // expected-error {{incorrect argument labels in call (have 'aax:bbx:', expected 'aa:bb:')}} {{15-18=aa}} {{23-26=xx}}

  // CHECK: [[@LINE+1]]:124: error: expected no fix-its; actual fix-its seen: {{\{\{}}15-18=aa}} {{\{\{}}23-26=bb}}
  labeledFunc(aax: 0, bbx: 1) // expected-error {{incorrect argument labels in call (have 'aax:bbx:', expected 'aa:bb:')}} {{none}}

  // CHECK: [[@LINE+1]]:137: error: unexpected fix-it seen; actual fix-its seen: {{\{\{}}15-18=aa}} {{\{\{}}23-26=bb}}
  labeledFunc(aax: 0, bbx: 1) // expected-error {{incorrect argument labels in call (have 'aax:bbx:', expected 'aa:bb:')}} {{15-18=aa}} {{none}}

  labeledFunc(aax: 0, bbx: 1) // expected-error {{incorrect argument labels in call (have 'aax:bbx:', expected 'aa:bb:')}} {{15-18=aa}} {{23-26=bb}} {{none}}

  // CHECK: [[@LINE+1]]:137: error: expected fix-it not seen; actual fix-its seen: {{\{\{}}15-18=aa}} {{\{\{}}23-26=bb}}
  labeledFunc(aax: 0, bbx: 1) // expected-error {{incorrect argument labels in call (have 'aax:bbx:', expected 'aa:bb:')}} {{15-18=aa}} {{23-26=xx}} {{none}}
}
