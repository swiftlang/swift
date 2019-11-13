// RUN: rm -rf %t/stats-dir
// RUN: mkdir -p %t/stats-dir
// RUN: %target-typecheck-verify-swift
// RUN: not %target-swift-frontend -typecheck -debug-cycles %s -output-request-graphviz %t.dot -stats-output-dir %t/stats-dir 2> %t.cycles
// RUN: %FileCheck %s < %t.cycles
// RUN: %FileCheck -check-prefix CHECK-DOT %s  < %t.dot

// Check that we produced superclass type requests.
// RUN: %{python} %utils/process-stats-dir.py --evaluate 'SuperclassTypeRequest == 18' %t/stats-dir

class Left // expected-error {{circular reference}}
    : Right.Hand { // expected-note {{through reference here}}
  class Hand {}
}

class Right // expected-note {{through reference here}}
  : Left.Hand { // expected-note {{through reference here}}
  class Hand {}
}

class C : B { } // expected-error{{'C' inherits from itself}}
class B : A { } // expected-note{{class 'B' declared here}}
class A : C { } // expected-note{{class 'A' declared here}}

class TrivialCycle : TrivialCycle {} // expected-error{{'TrivialCycle' inherits from itself}}
protocol P : P {} // expected-error {{protocol 'P' refines itself}}

class Isomorphism : Automorphism { }
class Automorphism : Automorphism { } // expected-error{{'Automorphism' inherits from itself}}

// FIXME: Useless error
let _ = A() // expected-error{{'A' cannot be constructed because it has no accessible initializers}}

class Outer {
  class Inner : Outer {}
}

class Outer2 // expected-error {{circular reference}}
    : Outer2.Inner { // expected-note {{through reference here}}

  class Inner {}
}

class Outer3 // expected-error {{circular reference}}
    : Outer3.Inner<Int> { // expected-note {{through reference here}}
  class Inner<T> {}
}

// CHECK: ===CYCLE DETECTED===
// CHECK-NEXT: `--{{.*}}HasCircularInheritanceRequest(circular_inheritance.(file).Left@
// CHECK-NEXT:     `--{{.*}}SuperclassDeclRequest({{.*Left}}
// CHECK:          `--{{.*}}InheritedDeclsReferencedRequest(circular_inheritance.(file).Left@
// CHECK:              `--{{.*}}SuperclassDeclRequest
// CHECK:                  `--{{.*}}InheritedDeclsReferencedRequest(circular_inheritance.(file).Right@
// CHECK:                      `--{{.*}}SuperclassDeclRequest{{.*(cyclic dependency)}}

// CHECK-DOT: digraph Dependencies
// CHECK-DOT: label="InheritedTypeRequest

protocol Initable {
  init()
  // expected-note@-1 {{protocol requires initializer 'init()' with type '()'; do you want to add a stub?}}
  // expected-note@-2 {{did you mean 'init'?}}
}

protocol Shape : Circle {}

class Circle : Initable & Circle {}
// expected-error@-1 {{'Circle' inherits from itself}}
// expected-error@-2 {{type 'Circle' does not conform to protocol 'Initable'}}

func crash() {
  Circle()
  // expected-error@-1 {{'Circle' cannot be constructed because it has no accessible initializers}}
}

// FIXME: We shouldn't emit the redundant "circular reference" diagnostics here.
class WithDesignatedInit : WithDesignatedInit {
  // expected-error@-1 {{'WithDesignatedInit' inherits from itself}}
  // expected-error@-2 {{circular reference}}
  // expected-note@-3 {{through reference here}}

  init(x: Int) {} // expected-error {{circular reference}}
}
