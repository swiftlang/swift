// RUN: %target-typecheck-verify-swift \
// RUN:   -suppress-warnings -suppress-notes \
// RUN:   -I %S/Inputs -cxx-interoperability-mode=default

import MemberInline

protocol HasPointee { var pointee: CInt { get } }
func reqPointee<T: HasPointee>(_ _: T) {}

protocol HasSucc { func successor() -> Self }
func reqSucc<T: HasSucc>(_ _: T) {}

extension ClassWithOperatorStarAvailable : HasPointee {}
extension ClassWithOperatorStarAvailable : HasSucc {} // expected-error {{does not conform to protocol}}

extension DerivedClassWithOperatorStarAvailable : HasPointee {}
extension DerivedClassWithOperatorStarAvailable : HasSucc {} // expected-error {{does not conform to protocol}}

extension ClassWithOperatorStarUnavailable : HasPointee {} // expected-error {{does not conform to protocol}}
                                                           // expected-error@-1 {{unavailable property}}
extension ClassWithOperatorStarUnavailable : HasSucc {} // expected-error {{does not conform to protocol}}

// FIXME: The below test should also fail with 'pointee' is unavailable in Swift error,
// but currently pointee is not hidden in derived classes.
extension DerivedClassWithOperatorStarUnavailable : HasPointee {} // FIXME-error {{does not conform to protocol}}
                                                                  // FIXME-error@-1 {{unavailable property}}
extension DerivedClassWithOperatorStarUnavailable : HasSucc {} // expected-error {{does not conform to protocol}}

extension ClassWithSuccessorAvailable : HasPointee {} // expected-error {{does not conform to protocol}}
extension ClassWithSuccessorAvailable : HasSucc {}

extension ClassWithSuccessorUnavailable : HasPointee {} // expected-error {{does not conform to protocol}}
extension ClassWithSuccessorUnavailable : HasSucc {} // expected-error {{does not conform to protocol}}
                                                     // expected-error@-1 {{unavailable instance method}}
