// RUN: rm -rf %t/stats-dir
// RUN: mkdir -p %t/stats-dir
// RUN: %target-typecheck-verify-swift
// RUN: not %target-swift-frontend -typecheck -debug-cycles %s -build-request-dependency-graph -output-request-graphviz %t.dot -stats-output-dir %t/stats-dir 2> %t.cycles
// RUN: %FileCheck -check-prefix CHECK-DOT %s < %t.dot

class Left // expected-error {{'Left' inherits from itself}} expected-note 2{{through reference here}}
    : Right.Hand { // expected-note {{through reference here}}
  class Hand {}  // expected-note {{through reference here}}
}

class Right // expected-note 2 {{through reference here}} expected-note{{class 'Right' declared here}}
  : Left.Hand { // expected-note {{through reference here}}
  class Hand {}  // expected-error {{circular reference}}
}

class C : B { } // expected-error{{'C' inherits from itself}}
class B : A { } // expected-note{{class 'B' declared here}}
class A : C { } // expected-note{{class 'A' declared here}}

class TrivialCycle : TrivialCycle {} // expected-error{{'TrivialCycle' inherits from itself}}
protocol P : P {} // expected-error {{protocol 'P' refines itself}}

class Isomorphism : Automorphism { }
class Automorphism : Automorphism { } // expected-error{{'Automorphism' inherits from itself}}

let _ = A()

class Outer {
  class Inner : Outer {}
}

class Outer2 // expected-error {{'Outer2' inherits from itself}} expected-note 2 {{through reference here}}
    : Outer2.Inner { // expected-note {{through reference here}}

  class Inner {} // expected-error{{circular reference}}
}

class Outer3 // expected-error {{'Outer3' inherits from itself}} expected-note 2 {{through reference here}}
    : Outer3.Inner<Int> { // expected-note {{through reference here}}
  class Inner<T> {} // expected-error{{circular reference}}
}

// CHECK-DOT: digraph Dependencies
// CHECK-DOT: label="InheritedTypeRequest

protocol Initable {
  init()
}

protocol Shape : Circle {}

class Circle : Initable & Circle {}
// expected-error@-1 {{'Circle' inherits from itself}}
// expected-error@-2 {{initializer requirement 'init()' can only be satisfied by a 'required' initializer in non-final class 'Circle'}}

func crash() {
  _ = Circle()
}

class WithDesignatedInit : WithDesignatedInit {
  // expected-error@-1 {{'WithDesignatedInit' inherits from itself}}

  init(x: Int) {}
}
