// RUN: %empty-directory(%t)
//
// RUN: %target-build-swift -enable-experimental-feature BuiltinModule \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -enable-experimental-feature AnyAppleOSAvailability \
// RUN:   -Xfrontend -verify-type-layout -Xfrontend LoanPointer \
// RUN:   -Xfrontend -verify-type-layout -Xfrontend OptionalLoanPointer \
// RUN:   -Xfrontend -verify-type-layout -Xfrontend Optional2LoanPointer \
// RUN:   -Xfrontend -verify-type-layout -Xfrontend LoanNPEnum \
// RUN:   -Xfrontend -verify-type-layout -Xfrontend OptionalLoanNPEnum \
// RUN:   -Xfrontend -verify-type-layout -Xfrontend Optional2LoanNPEnum \
// RUN:   -Xfrontend -verify-type-layout -Xfrontend LoanAFD \
// RUN:   -Xfrontend -verify-type-layout -Xfrontend OptionalLoanAFD \
// RUN:   -Xfrontend -verify-type-layout -Xfrontend Optional2LoanAFD \
// RUN:   -Xfrontend -verify-type-layout -Xfrontend LoanNBB \
// RUN:   -Xfrontend -verify-type-layout -Xfrontend OptionalLoanNBB \
// RUN:   -Xfrontend -verify-type-layout -Xfrontend Optional2LoanNBB \
// RUN:   -Xfrontend -verify-type-layout -Xfrontend LoanNotQuiteBig \
// RUN:   -Xfrontend -verify-type-layout -Xfrontend OptionalLoanNotQuiteBig \
// RUN:   -Xfrontend -verify-type-layout -Xfrontend Optional2LoanNotQuiteBig \
// RUN:   -Xfrontend -verify-type-layout -Xfrontend LoanAlmostBig \
// RUN:   -Xfrontend -verify-type-layout -Xfrontend OptionalLoanAlmostBig \
// RUN:   -Xfrontend -verify-type-layout -Xfrontend Optional2LoanAlmostBig \
// RUN:   -Xfrontend -verify-type-layout -Xfrontend LoanBig \
// RUN:   -Xfrontend -verify-type-layout -Xfrontend OptionalLoanBig \
// RUN:   -Xfrontend -verify-type-layout -Xfrontend Optional2LoanBig \
// RUN:   -Xfrontend -verify-type-layout -Xfrontend LoanGrainy \
// RUN:   -Xfrontend -verify-type-layout -Xfrontend OptionalLoanGrainy \
// RUN:   -Xfrontend -verify-type-layout -Xfrontend Optional2LoanGrainy \
// RUN:   -o %t/a.out \
// RUN:   %s
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out 2>&1 | %FileCheck %s

// Type layout verifier is only compiled into the runtime in asserts builds.
// REQUIRES: swift_stdlib_asserts

// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
//
// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_BuiltinModule
// REQUIRES: swift_feature_AnyAppleOSAvailability

// CHECK-NOT: Type verification

import Builtin

struct Loan<T: ~Copyable>: ~Escapable {
	var value: Builtin.Borrow<T>

	@_lifetime(immortal) init() { fatalError() }
}

// Typealiases for the type verifier:

typealias URP = UnsafeRawPointer

// RawPointer has one extra inhabitant, uses the value representation for
// Borrow, so Borrow<RawPointer> should have one extra inhabitant
typealias LoanPointer = Loan<UnsafeRawPointer>
typealias OptionalLoanPointer = Optional<Loan<UnsafeRawPointer>>
typealias Optional2LoanPointer = Optional<Optional<Loan<UnsafeRawPointer>>>

// No-payload enum should have 256 - 3 = 253 extra inhabitants and use the
// Borrow value representation
enum NPEnum { case a, b, c }

typealias LoanNPEnum = Loan<NPEnum>
typealias OptionalLoanNPEnum = Optional<Loan<NPEnum>>
typealias Optional2LoanNPEnum = Optional<Optional<Loan<NPEnum>>>

// Addressable-for-dependencies type, uses the pointer borrow representation,
// which should have one extra inhabitant 
@available(anyAppleOS 26, *)
struct AFD { var x: [1 of Int] }

@available(anyAppleOS 26, *)
typealias LoanAFD = Loan<AFD>
@available(anyAppleOS 26, *)
typealias OptionalLoanAFD = Optional<Loan<AFD>>
@available(anyAppleOS 26, *)
typealias Optional2LoanAFD = Optional<Optional<Loan<AFD>>>

// Non-bitwise-borrowable type, uses the pointer borrow representation,
// which should have one extra inhabitant 
struct NBB { weak var x: AnyObject?; var y: Int }

typealias LoanNBB = Loan<NBB>
typealias OptionalLoanNBB = Optional<Loan<NBB>>
typealias Optional2LoanNBB = Optional<Optional<Loan<NBB>>>

// Below the "big" type threshold, still uses the value representation
struct NotQuiteBig { var x, y, z: UnsafeRawPointer }

typealias LoanNotQuiteBig = Loan<NotQuiteBig>
typealias OptionalLoanNotQuiteBig = Optional<Loan<NotQuiteBig>>
typealias Optional2LoanNotQuiteBig = Optional<Optional<Loan<NotQuiteBig>>>

// Just at the "big" type threshold, still uses the value representation
struct AlmostBig { var x, y, z, w: UnsafeRawPointer }

typealias LoanAlmostBig = Loan<AlmostBig>
typealias OptionalLoanAlmostBig = Optional<Loan<AlmostBig>>
typealias Optional2LoanAlmostBig = Optional<Optional<Loan<AlmostBig>>>

// Over the "big" type threshold, uses the pointer representation
struct Big { var x, y, z, w, v: UnsafeRawPointer }

typealias LoanBig = Loan<Big>
typealias OptionalLoanBig = Optional<Loan<Big>>
typealias Optional2LoanBig = Optional<Optional<Loan<Big>>>

struct Grainy { var x, y, z, w, v: Bool }

typealias LoanGrainy = Loan<Grainy>
typealias OptionalLoanGrainy = Optional<Loan<Grainy>>
typealias Optional2LoanGrainy = Optional<Optional<Loan<Grainy>>>

// CHECK-NOT: *** Type verification
// CHECK: ok!
print("ok!")
