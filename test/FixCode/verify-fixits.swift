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
  labeledFunc(0, bb: 1) // expected-error {{missing argument label 'aa:' in call}}

  labeledFunc(0, bb: 1) // expected-error {{missing argument label 'aa:' in call}} {{15-15=aa: }}

  labeledFunc(0, bb: 1) // expected-error {{missing argument label 'aa:' in call}} {{15-15=xx: }}

  labeledFunc(0, bb: 1) // expected-error {{missing argument label 'aa:' in call}} {{15-15=aa: }} {{15-15=xx: }}

  labeledFunc(0, bb: 1) // expected-error {{missing argument label 'aa:' in call}} {{15-15=xx: }} {{15-15=aa: }}

  labeledFunc(0, bb: 1) // expected-error {{missing argument label 'aa:' in call}} {{none}}

  labeledFunc(0, bb: 1) // expected-error {{missing argument label 'aa:' in call}} {{15-15=aa: }} {{none}}

  labeledFunc(0, bb: 1) // expected-error {{missing argument label 'aa:' in call}} {{15-15=xx: }} {{none}}

  labeledFunc(0, bb: 1) // expected-error {{missing argument label 'aa:' in call}} {{15-15=aa: }} {{15-15=xx: }} {{none}}

  labeledFunc(0, bb: 1) // expected-error {{missing argument label 'aa:' in call}} {{15-15=xx: }} {{15-15=aa: }} {{none}}
}

func test2Fixits() {
  labeledFunc(0, 1) // expected-error {{missing argument labels 'aa:bb:' in call}}

  labeledFunc(0, 1) // expected-error {{missing argument labels 'aa:bb:' in call}} {{15-15=aa: }}

  labeledFunc(0, 1) // expected-error {{missing argument labels 'aa:bb:' in call}} {{15-15=xx: }}

  labeledFunc(0, 1) // expected-error {{missing argument labels 'aa:bb:' in call}} {{15-15=aa: }} {{18-18=bb: }}

  labeledFunc(0, 1) // expected-error {{missing argument labels 'aa:bb:' in call}} {{15-15=aa: }} {{18-18=xx: }}

  labeledFunc(0, 1) // expected-error {{missing argument labels 'aa:bb:' in call}} {{none}}

  labeledFunc(0, 1) // expected-error {{missing argument labels 'aa:bb:' in call}} {{15-15=aa: }} {{none}}

  labeledFunc(0, 1) // expected-error {{missing argument labels 'aa:bb:' in call}} {{15-15=aa: }} {{18-18=bb: }} {{none}}

  labeledFunc(0, 1) // expected-error {{missing argument labels 'aa:bb:' in call}} {{15-15=aa: }} {{18-18=xx: }} {{none}}
}
