// RUN: %target-swift-frontend -emit-sil -swift-version 6 -target %target-swift-5.1-abi-triple -verify -verify-additional-prefix old- %s -o /dev/null
// RUN: %target-swift-frontend -emit-sil -swift-version 6 -target %target-swift-5.1-abi-triple -verify -verify-additional-prefix new- -enable-experimental-feature WeakLet %s -o /dev/null

// This test validates the behavior of transfer non sendable around ownership
// constructs like non copyable types, consuming/borrowing parameters, and inout
// parameters.

// REQUIRES: concurrency

final class S: Sendable {
  func foo() {}
}

// expected-old-note@+2 13{{class 'NS' does not conform to the 'Sendable' protocol}}
// expected-new-note@+1 12{{class 'NS' does not conform to the 'Sendable' protocol}}
final class NS {
  func bar() {}
}

func getS() -> S { S() }
func getNS() -> NS { NS() }

final class CheckOptionality1: Sendable {
  // expected-old-error@+4 {{'weak' variable should have optional type 'S?'}}
  // expected-old-error@+3 {{stored property 'x' of 'Sendable'-conforming class 'CheckOptionality1' is mutable}}
  // expected-new-error@+2 {{'weak' variable should have optional type 'S?'}}
  // expected-new-error@+1 {{stored property 'x' of 'Sendable'-conforming class 'CheckOptionality1' is mutable}}
  weak var x: S = getS()
}

final class CheckOptionality2: Sendable {
  // expected-old-error@+3 {{'weak' must be a mutable variable, because it may change at runtime}}
  // expected-old-error@+2 {{'weak' variable should have optional type 'S?'}}
  // expected-new-error@+1 {{'weak' variable should have optional type 'S?'}}
  weak let x: S = getS()
}

final class CheckSendability1: Sendable {
  // expected-old-error@+2 {{stored property 'x' of 'Sendable'-conforming class 'CheckSendability1' is mutable}}
  // expected-new-error@+1 {{stored property 'x' of 'Sendable'-conforming class 'CheckSendability1' is mutable}}
  weak var x: S? = nil

  weak var y: S? {
    get { x }
    set { x = newValue }
  }
}

final class CheckSendability2: Sendable {
  // expected-old-error@+2 {{stored property 'x' of 'Sendable'-conforming class 'CheckSendability2' is mutable}}
  // expected-new-error@+1 {{stored property 'x' of 'Sendable'-conforming class 'CheckSendability2' is mutable}}
  weak var x: NS? = nil
}

final class CheckSendability3: Sendable {
  // expected-old-error@+1 {{'weak' must be a mutable variable, because it may change at runtime}}
  weak let x: S? = nil
}

final class CheckSendability4: Sendable {
  // expected-old-error@+3 {{'weak' must be a mutable variable, because it may change at runtime}}
  // expected-old-error@+2 {{stored property 'x' of 'Sendable'-conforming class 'CheckSendability4' contains non-Sendable type 'NS'}}
  // expected-new-error@+1 {{stored property 'x' of 'Sendable'-conforming class 'CheckSendability4' contains non-Sendable type 'NS'}}
  weak let x: NS? = nil
}

final class CheckSendability5: Sendable {
  // expected-old-error@+2 {{stored property 'x' of 'Sendable'-conforming class 'CheckSendability5' is mutable}}
  // expected-new-error@+1 {{stored property 'x' of 'Sendable'-conforming class 'CheckSendability5' is mutable}}
  unowned var x: S = getS()
}

final class CheckSendability6: Sendable {
  // expected-old-error@+2 {{stored property 'x' of 'Sendable'-conforming class 'CheckSendability6' is mutable}}
  // expected-new-error@+1 {{stored property 'x' of 'Sendable'-conforming class 'CheckSendability6' is mutable}}
  unowned var x: NS = getNS()
}

final class CheckSendability7: Sendable {
  unowned let x: S = getS()
}

final class CheckSendability8: Sendable {
  // expected-old-error@+2 {{stored property 'x' of 'Sendable'-conforming class 'CheckSendability8' has non-sendable type 'NS'}}
  // expected-new-error@+1 {{stored property 'x' of 'Sendable'-conforming class 'CheckSendability8' has non-sendable type 'NS'}}
  unowned let x: NS = getNS()
}

final class CheckSendability9: Sendable {
  // expected-old-error@+2 {{stored property 'x' of 'Sendable'-conforming class 'CheckSendability9' is mutable}}
  // expected-new-error@+1 {{stored property 'x' of 'Sendable'-conforming class 'CheckSendability9' is mutable}}
  unowned(unsafe) var x: S = getS()
}

final class CheckSendability10: Sendable {
  // expected-old-error@+2 {{stored property 'x' of 'Sendable'-conforming class 'CheckSendability10' is mutable}}
  // expected-new-error@+1 {{stored property 'x' of 'Sendable'-conforming class 'CheckSendability10' is mutable}}
  unowned(unsafe) var x: NS = getNS()
}

final class CheckSendability11: Sendable {
  unowned(unsafe) let x: S = getS()
}

final class CheckSendability12: Sendable {
  // expected-old-error@+2 {{stored property 'x' of 'Sendable'-conforming class 'CheckSendability12' has non-sendable type 'NS'}}
  // expected-new-error@+1 {{stored property 'x' of 'Sendable'-conforming class 'CheckSendability12' has non-sendable type 'NS'}}
  unowned(unsafe) let x: NS = getNS()
}


func checkWeakCapture1(_ strongRef: S) -> @Sendable () -> Void {
  weak var weakRef: S? = strongRef
  return {
    // expected-old-error@+2 {{reference to captured var 'weakRef' in concurrently-executing code}}
    // expected-new-error@+1 {{reference to captured var 'weakRef' in concurrently-executing code}}
    weakRef?.foo()
  }
}

func checkWeakCapture2(_ strongRef: S) -> @Sendable () -> Void {
  // expected-old-error@+1 {{'weak' must be a mutable variable, because it may change at runtime}}
  weak let weakRef: S? = strongRef
  return {
    weakRef?.foo()
  }
}

func checkWeakCapture3(_ strongRef: S) -> @Sendable () -> Void {
  return { [weak weakRef = strongRef] in
    // TODO: Add expected old error, when https://github.com/swiftlang/swift/issues/80014 is fixed
    // See also https://forums.swift.org/t/lets-debug-missing-rbi-data-race-diagnostics/78910
    weakRef?.foo()
    // expected-new-error@+1 {{cannot assign to value: 'weakRef' is an immutable capture}}
    weakRef = nil
  }
}

func checkWeakCapture4(_ strongRef: NS) -> @Sendable () -> Void {
  // TODO: warning consider changing to 'let' constant
  weak var weakRef: NS? = strongRef
  return {
    // expected-old-error@+4 {{capture of 'weakRef' with non-sendable type 'NS?' in a '@Sendable' closure}}
    // expected-new-error@+3 {{capture of 'weakRef' with non-sendable type 'NS?' in a '@Sendable' closure}}
    // expected-old-error@+2 {{reference to captured var 'weakRef' in concurrently-executing code}}
    // expected-new-error@+1 {{reference to captured var 'weakRef' in concurrently-executing code}}
    weakRef?.bar()
  }
}

func checkWeakCapture5(_ strongRef: NS) -> @Sendable () -> Void {
  // expected-old-error@+1 {{'weak' must be a mutable variable, because it may change at runtime}}
  weak let weakRef: NS? = strongRef
  return {
    // expected-old-error@+2 {{capture of 'weakRef' with non-sendable type 'NS?' in a '@Sendable' closure}}
    // expected-new-error@+1 {{capture of 'weakRef' with non-sendable type 'NS?' in a '@Sendable' closure}}
    weakRef?.bar()
  }
}

func checkWeakCapture6(_ strongRef: NS) -> @Sendable () -> Void {
  return { [weak weakRef = strongRef] in
    // expected-old-error@+3 {{capture of 'weakRef' with non-sendable type 'NS?' in a '@Sendable' closure}}
    // For some reason the next error masks this error.
    // This case is split into two, to verify that when unmasked error is triggered.
    weakRef?.bar()
    // expected-new-error@+1 {{cannot assign to value: 'weakRef' is an immutable capture}}
    weakRef = nil
  }
}

func checkWeakCapture7(_ strongRef: NS) -> @Sendable () -> Void {
  return { [weak weakRef = strongRef] in
    // expected-old-error@+2 {{capture of 'weakRef' with non-sendable type 'NS?' in a '@Sendable' closure}}
    // expected-new-error@+1 {{capture of 'weakRef' with non-sendable type 'NS?' in a '@Sendable' closure}}
    weakRef?.bar()
  }
}

func checkUnownedCapture1(_ strongRef: S) -> @Sendable () -> Void {
  // expected-old-warning@+2 {{variable 'unownedRef' was never mutated; consider changing to 'let' constant}}
  // expected-new-warning@+1 {{variable 'unownedRef' was never mutated; consider changing to 'let' constant}}
  unowned var unownedRef: S = strongRef
  return {
    // expected-old-error@+2 {{reference to captured var 'unownedRef' in concurrently-executing code}}
    // expected-new-error@+1 {{reference to captured var 'unownedRef' in concurrently-executing code}}
    unownedRef.foo()
  }
}

func checkUnownedCapture2(_ strongRef: S) -> @Sendable () -> Void {
  unowned let unownedRef: S = strongRef
  return {
    unownedRef.foo()
  }
}

func checkUnownedCapture3(_ strongRef: S) -> @Sendable () -> Void {
  return { [unowned unownedRef = strongRef] in
    // TODO: Add expected old error, when https://github.com/swiftlang/swift/issues/80014 is fixed
    // See also https://forums.swift.org/t/lets-debug-missing-rbi-data-race-diagnostics/78910
    unownedRef.foo()
    // expected-old-error@+2 {{cannot assign to value: 'unownedRef' is an immutable capture}}
    // expected-new-error@+1 {{cannot assign to value: 'unownedRef' is an immutable capture}}
    unownedRef = strongRef
  }
}

func checkUnownedCapture4(_ strongRef: NS) -> @Sendable () -> Void {
  // expected-old-warning@+2 {{variable 'unownedRef' was never mutated; consider changing to 'let' constant}}
  // expected-new-warning@+1 {{variable 'unownedRef' was never mutated; consider changing to 'let' constant}}
  unowned var unownedRef: NS = strongRef
  return {
    // expected-old-error@+4 {{capture of 'unownedRef' with non-sendable type 'NS' in a '@Sendable' closure}}
    // expected-new-error@+3 {{capture of 'unownedRef' with non-sendable type 'NS' in a '@Sendable' closure}}
    // expected-old-error@+2 {{reference to captured var 'unownedRef' in concurrently-executing code}}
    // expected-new-error@+1 {{reference to captured var 'unownedRef' in concurrently-executing code}}
    unownedRef.bar()
  }
}

func checkUnownedCapture5(_ strongRef: NS) -> @Sendable () -> Void {
  unowned let unownedRef: NS = strongRef
  return {
    // expected-old-error@+2 {{capture of 'unownedRef' with non-sendable type 'NS' in a '@Sendable' closure}}
    // expected-new-error@+1 {{capture of 'unownedRef' with non-sendable type 'NS' in a '@Sendable' closure}}
    unownedRef.bar()
  }
}

func checkUnownedCapture6(_ strongRef: NS) -> @Sendable () -> Void {
  return { [unowned unownedRef = strongRef] in
    // For some reason the next error masks this error.
    // This case is split into two, to verify that when unmasked error is triggered.
    unownedRef.bar()
    // expected-old-error@+2 {{cannot assign to value: 'unownedRef' is an immutable capture}}
    // expected-new-error@+1 {{cannot assign to value: 'unownedRef' is an immutable capture}}
    unownedRef = strongRef
  }
}

func checkUnownedCapture7(_ strongRef: NS) -> @Sendable () -> Void {
  return { [unowned unownedRef = strongRef] in
    // expected-old-error@+2 {{capture of 'unownedRef' with non-sendable type 'NS' in a '@Sendable' closure}}
    // expected-new-error@+1 {{capture of 'unownedRef' with non-sendable type 'NS' in a '@Sendable' closure}}
    unownedRef.bar()
  }
}

func checkUnsafeCapture1(_ strongRef: S) -> @Sendable () -> Void {
  // expected-old-warning@+2 {{variable 'unownedRef' was never mutated; consider changing to 'let' constant}}
  // expected-new-warning@+1 {{variable 'unownedRef' was never mutated; consider changing to 'let' constant}}
  unowned(unsafe) var unownedRef: S = strongRef
  return {
    // expected-old-error@+2 {{reference to captured var 'unownedRef' in concurrently-executing code}}
    // expected-new-error@+1 {{reference to captured var 'unownedRef' in concurrently-executing code}}
    unownedRef.foo()
  }
}

func checkUnsafeCapture2(_ strongRef: S) -> @Sendable () -> Void {
  unowned(unsafe) let unownedRef: S = strongRef
  return {
    unownedRef.foo()
  }
}

func checkUnsafeCapture3(_ strongRef: S) -> @Sendable () -> Void {
  return { [unowned(unsafe) unownedRef = strongRef] in
    // TODO: Add expected old error, when https://github.com/swiftlang/swift/issues/80014 is fixed
    // See also https://forums.swift.org/t/lets-debug-missing-rbi-data-race-diagnostics/78910
    unownedRef.foo()
    // expected-old-error@+2 {{cannot assign to value: 'unownedRef' is an immutable capture}}
    // expected-new-error@+1 {{cannot assign to value: 'unownedRef' is an immutable capture}}
    unownedRef = strongRef
  }
}

func checkUnsafeCapture4(_ strongRef: NS) -> @Sendable () -> Void {
  // expected-old-warning@+2 {{variable 'unownedRef' was never mutated; consider changing to 'let' constant}}
  // expected-new-warning@+1 {{variable 'unownedRef' was never mutated; consider changing to 'let' constant}}
  unowned(unsafe) var unownedRef: NS = strongRef
  return {
    // expected-old-error@+4 {{capture of 'unownedRef' with non-sendable type 'NS' in a '@Sendable' closure}}
    // expected-new-error@+3 {{capture of 'unownedRef' with non-sendable type 'NS' in a '@Sendable' closure}}
    // expected-old-error@+2 {{reference to captured var 'unownedRef' in concurrently-executing code}}
    // expected-new-error@+1 {{reference to captured var 'unownedRef' in concurrently-executing code}}
    unownedRef.bar()
  }
}

func checkUnsafeCapture5(_ strongRef: NS) -> @Sendable () -> Void {
  unowned(unsafe) let unownedRef: NS = strongRef
  return {
    // expected-old-error@+2 {{capture of 'unownedRef' with non-sendable type 'NS' in a '@Sendable' closure}}
    // expected-new-error@+1 {{capture of 'unownedRef' with non-sendable type 'NS' in a '@Sendable' closure}}
    unownedRef.bar()
  }
}

func checkUnsafeCapture6(_ strongRef: NS) -> @Sendable () -> Void {
  return { [unowned(unsafe) unownedRef = strongRef] in
    // For some reason the next error masks this error.
    // This case is split into two, to verify that when unmasked error is triggered.
    unownedRef.bar()
    // expected-old-error@+2 {{cannot assign to value: 'unownedRef' is an immutable capture}}
    // expected-new-error@+1 {{cannot assign to value: 'unownedRef' is an immutable capture}}
    unownedRef = strongRef
  }
}

func checkUnsafeCapture7(_ strongRef: NS) -> @Sendable () -> Void {
  return { [unowned(unsafe) unownedRef = strongRef] in
    // expected-old-error@+2 {{capture of 'unownedRef' with non-sendable type 'NS' in a '@Sendable' closure}}
    // expected-new-error@+1 {{capture of 'unownedRef' with non-sendable type 'NS' in a '@Sendable' closure}}
    unownedRef.bar()
  }
}
