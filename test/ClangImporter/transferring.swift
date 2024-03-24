// RUN: %target-swift-frontend -swift-version 6 -disable-availability-checking -emit-sil -o /dev/null %s -parse-as-library -enable-experimental-feature TransferringArgsAndResults -verify -import-objc-header %S/Inputs/transferring.h

// REQUIRES: concurrency
// REQUIRES: asserts

////////////////////////
// MARK: Declarations //
////////////////////////

// Make our non sendable c struct non-Sendable
@available(*, unavailable)
extension NonSendableCStruct: Sendable {}

@MainActor func transferToMain<T>(_ t: T) async {}
func useValue<T>(_ t: T) {}

/////////////////
// MARK: Tests //
/////////////////

func funcTestTransferringResult() async {
  let x = NonSendableCStruct()
  let y = transferUserDefinedFromGlobalFunction(x)
  await transferToMain(x)
  useValue(y)

  // Just to show that without the transferring param, we generate diagnostics.
  let x2 = NonSendableCStruct()
  let y2 = returnUserDefinedFromGlobalFunction(x2)
  await transferToMain(x2) // expected-error {{transferring value of non-Sendable type 'NonSendableCStruct' from nonisolated context to main actor-isolated context}}
  useValue(y2) // expected-note {{use here could race}}
}

func funcTestTransferringArg() async {
  let x = NonSendableCStruct()
  transferUserDefinedIntoGlobalFunction(x) // expected-error {{value of non-Sendable type 'NonSendableCStruct' accessed after being transferred; later accesses could race}}
  useValue(x) // expected-note {{use here could race}}
}
