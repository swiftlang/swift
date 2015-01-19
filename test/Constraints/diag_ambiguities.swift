// RUN: %target-parse-verify-swift

func f0(i: Int, d: Double) {} // expected-note{{found this candidate}}
func f0(d: Double, i: Int) {} // expected-note{{found this candidate}}

f0(1, 2) // expected-error{{ambiguous use of 'f0'}}

func f1(i: Int16) {} // expected-note{{found this candidate}}
func f1(i: Int32) {} // expected-note{{found this candidate}}

f1(0) // expected-error{{ambiguous use of 'f1'}}

infix operator +++ { } 

func +++(i: Int, d: Double) {} // expected-note{{found this candidate}}
func +++(d: Double, i: Int) {} // expected-note{{found this candidate}}

1 +++ 2 // expected-error{{ambiguous use of operator '+++'}}

