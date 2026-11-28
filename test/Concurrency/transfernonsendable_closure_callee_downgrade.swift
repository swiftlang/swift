// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-sil -swift-version 6 -target %target-swift-5.1-abi-triple -module-name Test -verify -verify-additional-prefix default- %s -o /dev/null

// RUN: echo "---" > %t/blocklist.yml
// RUN: echo "DowngradeClosureCalleeRegionErrors:" >> %t/blocklist.yml
// RUN: echo "  ModuleName:" >> %t/blocklist.yml
// RUN: echo "    - FooBar" >> %t/blocklist.yml
// RUN: %target-swift-frontend -emit-sil -swift-version 6 -target %target-swift-5.1-abi-triple -module-name Test -blocklist-file %t/blocklist.yml -verify -verify-additional-prefix default- %s -o /dev/null

// RUN: echo "    - Test" >> %t/blocklist.yml
// RUN: %target-swift-frontend -emit-sil -swift-version 6 -target %target-swift-5.1-abi-triple -module-name Test -blocklist-file %t/blocklist.yml -verify -verify-additional-prefix blocklisted- %s -o /dev/null

// REQUIRES: concurrency

// Older compilers did not merge a called closure with the arguments and results
// of its call. Make sure that errors about regions formed by that merge explain
// how to suppress them, and that they are downgraded to warnings in modules
// that the DowngradeClosureCalleeRegionErrors blocklist action applies to.
// Errors about other regions stay errors. Since we track this per region,
// errors that we found before about a region formed by the merge are treated
// the same way.

////////////////////////
// MARK: Declarations //
////////////////////////

class NonSendableKlass {
  var other: NonSendableKlass? = nil
}

@MainActor func transferToMain<T>(_ t: T) async {}
func useValue<T>(_ t: T) {}
func sendValue<T>(_ t: consuming sending T) {}
func mergeFn(_ a: NonSendableKlass, _ b: NonSendableKlass) {}

func makeMerger(_ x: NonSendableKlass) -> (NonSendableKlass) -> Void {
  return { mergeFn($0, x) }
}

/////////////////
// MARK: Tests //
/////////////////

func newUseAfterSend() async {
  let x = NonSendableKlass()
  let y = NonSendableKlass()
  let f = makeMerger(x)
  f(y)
  await transferToMain(y) // expected-blocklisted-warning {{sending 'y' risks causing data races}}
  // expected-default-error @-1 {{sending 'y' risks causing data races}}
  // expected-note @-2 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}
  // expected-default-note @-3 {{if this data race cannot be fixed yet, this error can be suppressed by compiling in the Swift 5 language mode or by storing the sent value in a 'nonisolated(unsafe)' variable}}
  // expected-blocklisted-note @-4 {{module 'Test' is blocklisted to downgrade errors found by merging the regions of called closures with the regions of their arguments and results to warnings}}
  useValue(x) // expected-note {{access can happen concurrently}}
}

// We found this error before, but the region of 'x' was formed by merging in a
// called closure.
func existingUseAfterSend() async {
  let x = NonSendableKlass()
  let y = NonSendableKlass()
  let f = makeMerger(x)
  f(y)
  await transferToMain(x) // expected-blocklisted-warning {{sending 'x' risks causing data races}}
  // expected-default-error @-1 {{sending 'x' risks causing data races}}
  // expected-note @-2 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}
  // expected-default-note @-3 {{if this data race cannot be fixed yet, this error can be suppressed by compiling in the Swift 5 language mode or by storing the sent value in a 'nonisolated(unsafe)' variable}}
  // expected-blocklisted-note @-4 {{module 'Test' is blocklisted to downgrade errors found by merging the regions of called closures with the regions of their arguments and results to warnings}}
  useValue(x) // expected-note {{access can happen concurrently}}
}

func newSendOfTaskIsolatedValue(_ f: (NonSendableKlass) -> Void) async {
  let x = NonSendableKlass()
  f(x)
  await transferToMain(x) // expected-blocklisted-warning {{sending 'x' risks causing data races}}
  // expected-default-error @-1 {{sending 'x' risks causing data races}}
  // expected-note @-2 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
  // expected-default-note @-3 {{if this data race cannot be fixed yet, this error can be suppressed by compiling in the Swift 5 language mode or by storing the sent value in a 'nonisolated(unsafe)' variable}}
  // expected-blocklisted-note @-4 {{module 'Test' is blocklisted to downgrade errors found by merging the regions of called closures with the regions of their arguments and results to warnings}}
}

// We found the error for 'z' before, but parameters start in the same region,
// so merging 'x' into the region of 'f' forms the region of 'z' as well.
func newAndExistingSendOfTaskIsolatedValue(_ f: (NonSendableKlass) -> Void, _ z: NonSendableKlass) async {
  let x = NonSendableKlass()
  f(x)
  await transferToMain(x) // expected-blocklisted-warning {{sending 'x' risks causing data races}}
  // expected-default-error @-1 {{sending 'x' risks causing data races}}
  // expected-note @-2 {{sending 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
  // expected-default-note @-3 {{if this data race cannot be fixed yet, this error can be suppressed by compiling in the Swift 5 language mode or by storing the sent value in a 'nonisolated(unsafe)' variable}}
  // expected-blocklisted-note @-4 {{module 'Test' is blocklisted to downgrade errors found by merging the regions of called closures with the regions of their arguments and results to warnings}}
  await transferToMain(z) // expected-blocklisted-warning {{sending 'z' risks causing data races}}
  // expected-default-error @-1 {{sending 'z' risks causing data races}}
  // expected-note @-2 {{sending 'z' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
  // expected-default-note @-3 {{if this data race cannot be fixed yet, this error can be suppressed by compiling in the Swift 5 language mode or by storing the sent value in a 'nonisolated(unsafe)' variable}}
  // expected-blocklisted-note @-4 {{module 'Test' is blocklisted to downgrade errors found by merging the regions of called closures with the regions of their arguments and results to warnings}}
}

// Errors about regions that were not formed by merging in a called closure
// stay errors.
func existingSendOfTaskIsolatedValue(_ z: NonSendableKlass) async {
  let f: (NonSendableKlass) -> Void = { _ in }
  let x = NonSendableKlass()
  f(x)
  await transferToMain(z) // expected-error {{sending 'z' risks causing data races}}
  // expected-note @-1 {{sending 'z' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

// Without the merge, this was a use after send error. Now it is an error for
// sending a task-isolated value at the same send. We only know that the sent
// region was formed by the merge when we find the error, not that the code had
// a later use that we rejected before. So, we treat this error like the errors
// that we find only because of the merge even though we already rejected the
// code.
func existingUseAfterSendNowSendOfTaskIsolatedValue(_ f: (NonSendableKlass) -> NonSendableKlass) {
  let x = NonSendableKlass()
  sendValue(f(x)) // expected-blocklisted-warning {{sending 'f' risks causing data races}}
  // expected-default-error @-1 {{sending 'f' risks causing data races}}
  // expected-note @-2 {{'f' is passed as a 'sending' parameter; Uses in callee may race with code in the current isolation context}}
  // expected-default-note @-3 {{if this data race cannot be fixed yet, this error can be suppressed by compiling in the Swift 5 language mode or by storing the sent value in a 'nonisolated(unsafe)' variable}}
  // expected-blocklisted-note @-4 {{module 'Test' is blocklisted to downgrade errors found by merging the regions of called closures with the regions of their arguments and results to warnings}}
  useValue(x)
}

actor Custom {
  var field = NonSendableKlass()

  func newSendingResult() -> sending NonSendableKlass {
    let x = NonSendableKlass()
    let y = NonSendableKlass()
    let f: (NonSendableKlass) -> Void = { x.other = $0 }
    f(y)
    field = y
    return x // expected-blocklisted-warning {{sending 'x' risks causing data races}}
    // expected-default-error @-1 {{sending 'x' risks causing data races}}
    // expected-note @-2 {{'self'-isolated 'x' cannot be a 'sending' result. 'self'-isolated uses may race with caller uses}}
    // expected-default-note @-3 {{if this data race cannot be fixed yet, this error can be suppressed by compiling in the Swift 5 language mode or by storing the sent value in a 'nonisolated(unsafe)' variable}}
    // expected-blocklisted-note @-4 {{module 'Test' is blocklisted to downgrade errors found by merging the regions of called closures with the regions of their arguments and results to warnings}}
  }

  // We found the error for 'self.field' before. We find the error for 'f' only
  // because we merge the callee into the 'sending' indirect result.
  func newAndExistingAssignIntoSendingResult<T>(_ f: (NonSendableKlass) -> T) -> sending T {
    return f(field)
    // expected-error @-1 {{returning 'self'-isolated 'self.field' as a 'sending' result risks causing data races; 'self.field' is accessible to code in the current isolation context}}
    // expected-note @-2 {{returning 'self'-isolated 'self.field' risks causing data races since the caller assumes that 'self.field' can be safely sent to other isolation domains, but 'self.field' is accessible to code in the current isolation context}}
    // expected-blocklisted-warning @-3 {{returning 'self'-isolated 'f' as a 'sending' result risks causing data races; 'f' is accessible to code in the current isolation context}}
    // expected-default-error @-4 {{returning 'self'-isolated 'f' as a 'sending' result risks causing data races; 'f' is accessible to code in the current isolation context}}
    // expected-note @-5 {{returning 'self'-isolated 'f' risks causing data races since the caller assumes that 'f' can be safely sent to other isolation domains, but 'f' is accessible to code in the current isolation context}}
    // expected-default-note @-6 {{if this data race cannot be fixed yet, this error can be suppressed by compiling in the Swift 5 language mode or by storing the sent value in a 'nonisolated(unsafe)' variable}}
    // expected-blocklisted-note @-7 {{module 'Test' is blocklisted to downgrade errors found by merging the regions of called closures with the regions of their arguments and results to warnings}}
  }
}

// We find these errors only because we merge the callee into the 'sending'
// indirect result.
func newAssignIntoSendingResultNoArguments<T>(_ f: () -> T) -> sending T {
  return f() // expected-blocklisted-warning {{returning 'f' as a 'sending' result risks causing data races}}
  // expected-default-error @-1 {{returning 'f' as a 'sending' result risks causing data races}}
  // expected-note @-2 {{returning 'f' risks causing data races since the caller assumes that 'f' can be safely sent to other isolation domains}}
  // expected-default-note @-3 {{if this data race cannot be fixed yet, this error can be suppressed by compiling in the Swift 5 language mode or by storing the sent value in a 'nonisolated(unsafe)' variable}}
  // expected-blocklisted-note @-4 {{module 'Test' is blocklisted to downgrade errors found by merging the regions of called closures with the regions of their arguments and results to warnings}}
}

func newAssignIntoSendingResultDisconnectedArgument<T>(_ f: (NonSendableKlass) -> T) -> sending T {
  return f(NonSendableKlass()) // expected-blocklisted-warning {{returning 'f' as a 'sending' result risks causing data races}}
  // expected-default-error @-1 {{returning 'f' as a 'sending' result risks causing data races}}
  // expected-note @-2 {{returning 'f' risks causing data races since the caller assumes that 'f' can be safely sent to other isolation domains}}
  // expected-default-note @-3 {{if this data race cannot be fixed yet, this error can be suppressed by compiling in the Swift 5 language mode or by storing the sent value in a 'nonisolated(unsafe)' variable}}
  // expected-blocklisted-note @-4 {{module 'Test' is blocklisted to downgrade errors found by merging the regions of called closures with the regions of their arguments and results to warnings}}
}

// Calling the closure merges the isolation of the call into its region. We
// rejected this code before, but the region of 'f' was formed by merging in a
// called closure, so we treat it like the errors that we find only because of
// the merge.
func existingSendOfIsolatedParameterClosure(_ c: isolated Custom) {
  let f: (isolated Custom) -> Void = { _ in }
  f(c)
  sendValue(f) // expected-blocklisted-warning {{sending 'f' risks causing data races}}
  // expected-default-error @-1 {{sending 'f' risks causing data races}}
  // expected-note @-2 {{'c'-isolated 'f' is passed as a 'sending' parameter; Uses in callee may race with later 'c'-isolated uses}}
  // expected-default-note @-3 {{if this data race cannot be fixed yet, this error can be suppressed by compiling in the Swift 5 language mode or by storing the sent value in a 'nonisolated(unsafe)' variable}}
  // expected-blocklisted-note @-4 {{module 'Test' is blocklisted to downgrade errors found by merging the regions of called closures with the regions of their arguments and results to warnings}}
}

////////////////////////////
// MARK: Workaround Tests //
////////////////////////////

// Storing the sent value in a 'nonisolated(unsafe)' variable suppresses these
// errors as the note suggests.

func newUseAfterSendWorkaround() async {
  let x = NonSendableKlass()
  nonisolated(unsafe) let y = NonSendableKlass()
  let f = makeMerger(x)
  f(y)
  await transferToMain(y)
  useValue(x)
}

func newSendOfTaskIsolatedValueWorkaround(_ f: (NonSendableKlass) -> Void) async {
  nonisolated(unsafe) let x = NonSendableKlass()
  f(x)
  await transferToMain(x)
}

func existingUseAfterSendNowSendOfTaskIsolatedValueWorkaround(_ f: (NonSendableKlass) -> NonSendableKlass) {
  let x = NonSendableKlass()
  nonisolated(unsafe) let y = f(x)
  sendValue(y)
  useValue(x)
}

func newAssignIntoSendingResultNoArgumentsWorkaround<T>(_ f: () -> T) -> sending T {
  nonisolated(unsafe) let t = f()
  return t
}
