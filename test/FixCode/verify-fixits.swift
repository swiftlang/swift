// RUN: cp %s %t
// RUN: not %swift -typecheck -target %target-triple -verify-apply-fixes %t
// RUN: diff %t %s.result

func f1() {
  guard true { return } // expected-error {{...}}

  guard true { return } // expected-error {{expected 'else' after 'guard' condition}} {{none}}
}

func labeledFunc(aa: Int, bb: Int) {}

func test0Fixits() {
  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}}

  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1-1=a}}

  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1-1=a}} {{2-2=b}}

  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{none}}

  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1-1=a}} {{none}}

  undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}} {{1-1=a}} {{2-2=b}} {{none}}
}

func test1Fixits() {
  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}}

  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{15-18=aa}}

  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{15-18=xx}}

  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{15-18=aa}} {{15-18=xx}}

  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{15-18=xx}} {{15-18=aa}}

  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{none}}

  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{15-18=aa}} {{none}}

  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{15-18=xx}} {{none}}

  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{15-18=aa}} {{15-18=xx}} {{none}}

  labeledFunc(aax: 0, bb: 1) // expected-error {{incorrect argument label in call (have 'aax:bb:', expected 'aa:bb:')}} {{15-18=xx}} {{15-18=aa}} {{none}}
}

func test2Fixits() {
  labeledFunc(aax: 0, bbx: 1) // expected-error {{incorrect argument labels in call (have 'aax:bbx:', expected 'aa:bb:')}}

  labeledFunc(aax: 0, bbx: 1) // expected-error {{incorrect argument labels in call (have 'aax:bbx:', expected 'aa:bb:')}} {{15-18=aa}}

  labeledFunc(aax: 0, bbx: 1) // expected-error {{incorrect argument labels in call (have 'aax:bbx:', expected 'aa:bb:')}} {{15-18=xx}}

  labeledFunc(aax: 0, bbx: 1) // expected-error {{incorrect argument labels in call (have 'aax:bbx:', expected 'aa:bb:')}} {{15-18=aa}} {{23-26=bb}}

  labeledFunc(aax: 0, bbx: 1) // expected-error {{incorrect argument labels in call (have 'aax:bbx:', expected 'aa:bb:')}} {{15-18=aa}} {{23-26=xx}}

  labeledFunc(aax: 0, bbx: 1) // expected-error {{incorrect argument labels in call (have 'aax:bbx:', expected 'aa:bb:')}} {{none}}

  labeledFunc(aax: 0, bbx: 1) // expected-error {{incorrect argument labels in call (have 'aax:bbx:', expected 'aa:bb:')}} {{15-18=aa}} {{none}}

  labeledFunc(aax: 0, bbx: 1) // expected-error {{incorrect argument labels in call (have 'aax:bbx:', expected 'aa:bb:')}} {{15-18=aa}} {{23-26=bb}} {{none}}

  labeledFunc(aax: 0, bbx: 1) // expected-error {{incorrect argument labels in call (have 'aax:bbx:', expected 'aa:bb:')}} {{15-18=aa}} {{23-26=xx}} {{none}}
}
