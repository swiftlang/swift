// RUN: not %swift -typecheck -target %target-triple %s -fixit-all -emit-fixits-path %t.remap
// RUN: c-arcmt-test %t.remap | arcmt-test -verify-transformed-files %s.result

protocol P {}

// FIX-ME(SR-8102): Emit fix-its for the last requirements once we support bulk
// removal of 'dependant' fix-its.
struct S1<T> where T : Any, T : Any, T : Any {}
struct S2<T> where T : P, T : Any, T : Any {}
struct S3<T> where T : Any, T : P, T : Any {}
struct S4<T> where T : Any, T : Any, T : P {}
struct S5<T> where T : Any {}

protocol P1 {
  associatedtype X1 where X1 : Any
  associatedtype X2 where X2 : Any, X2 : Any
  associatedtype X3 where X2 : Any, X2 : Any, X2 : P
}

// FIX-ME(SR-8102): Always emit fix-its for the second conformances once we
// support bulk removal of 'dependant' fix-its.
struct S6 : Any, Any, Any {}
struct S7 : P, Any, Any {}
struct S8 : Any, P, Any {}
struct S9 : Any, Any, P {}
struct S10 : Any {}
struct S11 : Any, P {}

enum E1 : Any, String { case x }
enum E2 : String, Any { case x }
enum E3 : Any {}
enum E4 : Any, Any, String { case x }

class C {}
class C1 : Any, C {}
class C2 : C, Any {}
class C3 : Any {}
class C4 : Any, Any, C {}
class C5 : Any, AnyObject {}
class C6 : AnyObject, Any {}
class C7 : Any, Any, AnyObject {}

// FIX-ME: Implement redundant Any fix-its for requirements parsed as inheritance clauses.
struct S12<T : Any> {}
protocol P2 : Any {}
protocol P3 : Any, Any {}
protocol P4 : Any, class, AnyObject {} // Parse will re-arrange 'class' to be first.
protocol P5 : class, Any, AnyObject {}
protocol P6 : class, AnyObject, Any {}

protocol P7 {
  associatedtype X1 : Any
  associatedtype X2 : Any, Any
}
