// RUN: %target-typecheck-verify-swift -requirement-machine-protocol-signatures=on -requirement-machine-inferred-signatures=on

func testInvalidConformance() {
  // expected-error@+1 {{type 'T' constrained to non-protocol, non-class type 'Int'}}
  func invalidIntConformance<T>(_: T) where T: Int {}

  // expected-error@+1 {{type 'T' constrained to non-protocol, non-class type 'Int'}}
  struct InvalidIntConformance<T: Int> {}

  struct S<T> {
    // expected-error@+2 {{type 'T' constrained to non-protocol, non-class type 'Int'}}
    // expected-note@+1 {{use 'T == Int' to require 'T' to be 'Int'}}
    func method() where T: Int {}
  }
}

// Check directly-concrete same-type constraints
typealias NotAnInt = Double

protocol X {}

// expected-error@+1{{generic signature requires types 'Double' and 'Int' to be the same}}
extension X where NotAnInt == Int {}

protocol EqualComparable {
  func isEqual(_ other: Self) -> Bool
}

func badTypeConformance1<T>(_: T) where Int : EqualComparable {} // expected-error{{type 'Int' in conformance requirement does not refer to a generic parameter or associated type}}

func badTypeConformance2<T>(_: T) where T.Blarg : EqualComparable { } // expected-error{{'Blarg' is not a member type of type 'T'}}

func badTypeConformance3<T>(_: T) where (T) -> () : EqualComparable { }
// expected-error@-1{{type '(T) -> ()' in conformance requirement does not refer to a generic parameter or associated type}}

func badTypeConformance4<T>(_: T) where (inout T) throws -> () : EqualComparable { }
// expected-error@-1{{type '(inout T) throws -> ()' in conformance requirement does not refer to a generic parameter or associated type}}

func badTypeConformance5<T>(_: T) where T & Sequence : EqualComparable { }
// expected-error@-1 {{non-protocol, non-class type 'T' cannot be used within a protocol-constrained type}}

func badTypeConformance6<T>(_: T) where [T] : Collection { }
// expected-warning@-1{{redundant conformance constraint '[T]' : 'Collection'}}

func concreteSameTypeRedundancy<T>(_: T) where Int == Int {}
// expected-warning@-1{{redundant same-type constraint 'Int' == 'Int'}}

func concreteSameTypeRedundancy<T>(_: T) where Array<Int> == Array<T> {}
// expected-warning@-1{{redundant same-type constraint 'Array<Int>' == 'Array<T>'}}

protocol P {}
struct S: P {}
func concreteConformanceRedundancy<T>(_: T) where S : P {}
// expected-warning@-1{{redundant conformance constraint 'S' : 'P'}}

class C {}
func concreteLayoutRedundancy<T>(_: T) where C : AnyObject {}
// expected-warning@-1{{redundant constraint 'C' : 'AnyObject'}}

func concreteLayoutConflict<T>(_: T) where Int : AnyObject {}
// expected-error@-1{{type 'Int' in conformance requirement does not refer to a generic parameter or associated type}}

class C2: C {}
func concreteSubclassRedundancy<T>(_: T) where C2 : C {}
// expected-warning@-1{{redundant superclass constraint 'C2' : 'C'}}

class D {}
func concreteSubclassConflict<T>(_: T) where D : C {}
// expected-error@-1{{type 'D' in conformance requirement does not refer to a generic parameter or associated type}}

protocol UselessProtocolWhereClause where Int == Int {}
// expected-warning@-1 {{redundant same-type constraint 'Int' == 'Int'}}

protocol InvalidProtocolWhereClause where Self: Int {}
// expected-error@-1 {{type 'Self' constrained to non-protocol, non-class type 'Int'}}

typealias Alias<T> = T where Int == Int
// expected-warning@-1 {{redundant same-type constraint 'Int' == 'Int'}}

func cascadingConflictingRequirement<T>(_: T) where DoesNotExist : EqualComparable { }
// expected-error@-1 {{cannot find type 'DoesNotExist' in scope}}

func cascadingInvalidConformance<T>(_: T) where T : DoesNotExist { }
// expected-error@-1 {{cannot find type 'DoesNotExist' in scope}}
