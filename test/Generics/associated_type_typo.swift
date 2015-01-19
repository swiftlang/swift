// RUN: %target-parse-verify-swift

// RUN: not %target-swift-frontend -parse -debug-generic-signatures %s > %t.dump 2>&1 
// RUN: FileCheck -check-prefix CHECK-GENERIC %s < %t.dump

protocol P1 {
  typealias Assoc
}

protocol P2 {
  typealias AssocP2 : P1
}

protocol P3 { }
protocol P4 { }

// expected-error@+1{{'T' does not have a member type named 'assoc'; did you mean 'Assoc'?}}{{30-35=Assoc}}
func typoAssoc1<T : P1>(x: T.assoc) { } 

// expected-error@+1{{'T' does not have a member type named 'assoc'; did you mean 'Assoc'?}}{{40-45=Assoc}}{{51-56=Assoc}}
func typoAssoc2<T : P1, U : P1 where T.assoc == U.assoc>() { } 

// expected-error@+2{{'T.AssocP2' does not have a member type named 'assoc'; did you mean 'Assoc'?}}{{27-32=Assoc}}{{50-55=Assoc}}
func typoAssoc3<T : P2, U : P2 where 
                U.AssocP2.assoc : P3,  T.AssocP2.assoc : P4,
                T.AssocP2 == U.AssocP2>() { }

// expected-error@+2{{'T' does not have a member type named 'Assocp2'; did you mean 'AssocP2'?}}{{32-39=AssocP2}}
// expected-error@+1{{'T.AssocP2' does not have a member type named 'assoc'; did you mean 'Assoc'?}}{{40-45=Assoc}}
func typoAssoc4<T : P2 where T.Assocp2.assoc : P3>() { }

// CHECK-GENERIC-LABEL: .typoAssoc4()@
// CHECK-NEXT: Requirements:
// CHECK-NEXT:   T : P2 [explicit
// CHECK-NEXT:   T[.P2].AssocP2 == T.AssocP2 [protocol
// CHECK-NEXT:   T[.P2].AssocP2[.P1].Assoc : P3 [explicit
// CHECK-NEXT: Generic signature
