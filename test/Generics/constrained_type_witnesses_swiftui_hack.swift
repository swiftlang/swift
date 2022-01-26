// RUN: %target-typecheck-verify-swift -module-name SwiftUI

class Class {}
protocol P {
  associatedtype A
  // expected-note@-1 5{{protocol requires nested type 'A'; do you want to add it?}}
}

struct S1<T> {}
extension S1 where T : P {
  typealias A = Never // expected-note {{candidate would match if the conformance required that 'T' conformed to 'P'}}
}
extension S1 : P {} // expected-warning {{type 'S1<T>' does not conform to protocol 'P'}}

struct S2<T> {
  typealias A = Never where T : P // expected-note {{candidate would match if the conformance required that 'T' conformed to 'P'}}
}
extension S2 : P {} // expected-warning {{type 'S2<T>' does not conform to protocol 'P'}}

struct S3<T> {}
extension S3 where T : Class {
  typealias A = Never // expected-note {{candidate would match if the conformance required that 'T' subclassed 'Class'}}
}
extension S3 : P {} // expected-warning {{type 'S3<T>' does not conform to protocol 'P'}}

struct S4<T> {}
extension S4 where T == Bool {
  typealias A = Never // expected-note {{candidate would match if the conformance required that 'T' was the same type as 'Bool'}}
}
extension S4 : P {} // expected-warning {{type 'S4<T>' does not conform to protocol 'P'}}

struct S5<T> {}
extension S5 where T: AnyObject {
  typealias A = Never
}
extension S5 : P {} // expected-warning {{type 'S5<T>' does not conform to protocol 'P'}}
