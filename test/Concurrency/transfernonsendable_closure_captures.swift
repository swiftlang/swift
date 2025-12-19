// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -disable-availability-checking -verify %s -o /dev/null -swift-version 6

////////////////////////
// MARK: Declarations //
////////////////////////

class KlassNonsendable {}
class KlassSendable : @unchecked Sendable {}

struct NoncopyableStructNonsendable : ~Copyable {
  var ns = KlassNonsendable()
}
struct NoncopyableStructSendable : ~Copyable, Sendable {
  var s = KlassSendable()
}
struct CopyableStructNonsendable {
  var ns = KlassNonsendable()
}
struct CopyableStructSendable : Sendable {
  var s = KlassSendable()
}

func useValue<T : ~Copyable>(_ t: borrowing T) {}
func escapingAsyncUse(_ x: @escaping () async -> ()) {
}
func nonescapingAsyncUse(_ x: () async -> ()) {
}

//////////////////////////
// MARK: Copyable Tests //
//////////////////////////

//===---
// Escaping
//

func testCopyableSendableStructWithEscapingMainActorAsync() {
  let x = CopyableStructSendable()
  let _ = {
    escapingAsyncUse { @MainActor in
      useValue(x)
    }
  }
}

// TODO: We should say that it is due to the box being mutable.
func testMutableCopyableSendableStructWithEscapingMainActorAsync() {
  var x = CopyableStructSendable()
  x = CopyableStructSendable()
  let _ = {
    escapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

func testCopyableNonsendableStructWithEscapingMainActorAsync() {
  let x = CopyableStructNonsendable()
  let _ = {
    escapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

func testMutableCopyableNonsendableStructWithEscapingMainActorAsync() {
  var x = CopyableStructNonsendable()
  x = CopyableStructNonsendable()
  let _ = {
    escapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

//===---
// Nonescaping
//

// TODO: Study these.

func testCopyableSendableStructWithNonescapingMainActorAsync() {
  let x = CopyableStructSendable()
  let _ = {
    nonescapingAsyncUse { @MainActor in
      useValue(x)
    }
  }
}

func testMutableCopyableSendableStructWithNonescapingMainActorAsync() {
  var x = CopyableStructSendable()
  x = CopyableStructSendable()
  let _ = {
    nonescapingAsyncUse { @MainActor in
      useValue(x)
    }
  }
}

func testCopyableNonsendableStructWithNonescapingMainActorAsync() {
  let x = CopyableStructNonsendable()
  let _ = {
    nonescapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

func testMutableCopyableNonsendableStructWithNonescapingMainActorAsync() {
  var x = CopyableStructNonsendable()
  x = CopyableStructNonsendable()
  let _ = {
    nonescapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

/////////////////////////////
// MARK: Noncopyable Tests //
/////////////////////////////

func testNoncopyableSendableStructWithEscapingMainActorAsync() {
  let x = NoncopyableStructSendable()
  let _ = {
    escapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

func testMutableNoncopyableSendableStructWithEscapingMainActorAsync() {
  var x = NoncopyableStructSendable()
  x = NoncopyableStructSendable()
  let _ = {
    escapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

func testNoncopyableNonsendableStructWithEscapingMainActorAsync() {
  let x = NoncopyableStructNonsendable()
  let _ = {
    escapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

func testMutableNoncopyableNonsendableStructWithEscapingMainActorAsync() {
  var x = NoncopyableStructNonsendable()
  x = NoncopyableStructNonsendable()
  let _ = {
    escapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

func testNoncopyableSendableStructWithNonescapingMainActorAsync() {
  let x = NoncopyableStructSendable()
  let _ = {
    nonescapingAsyncUse { @MainActor in
      useValue(x)
    }
  }
}

func testMutableNoncopyableSendableStructWithNonescapingMainActorAsync() {
  var x = NoncopyableStructSendable()
  x = NoncopyableStructSendable()
  let _ = {
    nonescapingAsyncUse { @MainActor in
      useValue(x)
    }
  }
}

func testNoncopyableNonsendableStructWithNonescapingMainActorAsync() {
  let x = NoncopyableStructNonsendable()
  let _ = {
    nonescapingAsyncUse { @MainActor in
      useValue(x)  // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

func testMutableNoncopyableNonsendableStructWithNonescapingMainActorAsync() {
  var x = NoncopyableStructNonsendable()
  x = NoncopyableStructNonsendable()
  let _ = {
    nonescapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

//////////////////////////////////
// MARK: Normal Capture Lists //
//////////////////////////////////

func testCopyableSendableStructWithEscapingMainActorAsyncNormalCapture() {
  let x = CopyableStructSendable()
  let _ = { [x] in
    escapingAsyncUse { @MainActor in
      useValue(x)
    }
  }
}

func testMutableCopyableSendableStructWithEscapingMainActorAsyncNormalCapture() {
  var x = CopyableStructSendable()
  x = CopyableStructSendable()
  let _ = { [x] in
    escapingAsyncUse { @MainActor in
      useValue(x)
    }
  }
}

func testCopyableNonsendableStructWithEscapingMainActorAsyncNormalCapture() {
  let x = CopyableStructNonsendable()
  let _ = { [x] in
    escapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

func testMutableCopyableNonsendableStructWithEscapingMainActorAsyncNormalCapture() {
  var x = CopyableStructNonsendable()
  x = CopyableStructNonsendable()
  let _ = { [x] in
    escapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

func testNoncopyableSendableStructWithEscapingMainActorAsyncNormalCapture() {
  let x = NoncopyableStructSendable()
  let _ = { [x] in
    escapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

func testMutableNoncopyableSendableStructWithEscapingMainActorAsyncNormalCapture() {
  var x = NoncopyableStructSendable()
  x = NoncopyableStructSendable()
  let _ = { [x] in
    escapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

func testNoncopyableNonsendableStructWithEscapingMainActorAsyncNormalCapture() {
  let x = NoncopyableStructNonsendable()
  let _ = { [x] in
    escapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

func testMutableNoncopyableNonsendableStructWithEscapingMainActorAsyncNormalCapture() {
  var x = NoncopyableStructNonsendable()
  x = NoncopyableStructNonsendable()
  let _ = { [x] in
    escapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

func testCopyableSendableStructWithNonescapingMainActorAsyncNormalCapture() {
  let x = CopyableStructSendable()
  let _ = { [x] in
    nonescapingAsyncUse { @MainActor in
      useValue(x)
    }
  }
}

func testMutableCopyableSendableStructWithNonescapingMainActorAsyncNormalCapture() {
  var x = CopyableStructSendable()
  x = CopyableStructSendable()
  let _ = { [x] in
    nonescapingAsyncUse { @MainActor in
      useValue(x)
    }
  }
}

func testCopyableNonsendableStructWithNonescapingMainActorAsyncNormalCapture() {
  let x = CopyableStructNonsendable()
  let _ = { [x] in
    nonescapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

func testMutableCopyableNonsendableStructWithNonescapingMainActorAsyncNormalCapture() {
  var x = CopyableStructNonsendable()
  x = CopyableStructNonsendable()
  let _ = { [x] in
    nonescapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

func testNoncopyableSendableStructWithNonescapingMainActorAsyncNormalCapture() {
  let x = NoncopyableStructSendable()
  let _ = { [x] in
    nonescapingAsyncUse { @MainActor in
      useValue(x)
    }
  }
}

func testMutableNoncopyableSendableStructWithNonescapingMainActorAsyncNormalCapture() {
  var x = NoncopyableStructSendable()
  x = NoncopyableStructSendable()
  let _ = { [x] in
    nonescapingAsyncUse { @MainActor in
      useValue(x)
    }
  }
}
func testNoncopyableNonsendableStructWithNonescapingMainActorAsyncNormalCapture() {
  let x = NoncopyableStructNonsendable()
  let _ = { [x] in
    nonescapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

func testMutableNoncopyableNonsendableStructWithNonescapingMainActorAsyncNormalCapture() {
  var x = NoncopyableStructNonsendable()
  x = NoncopyableStructNonsendable()
  let _ = { [x] in
    nonescapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

///////////////////////////////
// MARK: Weak Capture Lists //
///////////////////////////////

func testCopyableSendableClassWithEscapingMainActorAsyncWeakCapture() {
  let x = KlassSendable()
  let _ = { [weak x] in
    escapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

func testMutableCopyableSendableClassWithEscapingMainActorAsyncWeakCapture() {
  var x: KlassSendable? = KlassSendable()
  x = KlassSendable()
  let _ = { [weak x] in
    escapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

func testCopyableNonsendableClassWithEscapingMainActorAsyncWeakCapture() {
  let x = KlassNonsendable()
  let _ = { [weak x] in
    escapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

func testMutableCopyableNonsendableClassWithEscapingMainActorAsyncWeakCapture() {
  var x: KlassNonsendable? = KlassNonsendable()
  x = KlassNonsendable()
  let _ = { [weak x] in
    escapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

func testCopyableSendableClassWithNonescapingMainActorAsyncWeakCapture() {
  let x = KlassSendable()
  let _ = { [weak x] in
    nonescapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

func testMutableCopyableSendableClassWithNonescapingMainActorAsyncWeakCapture() {
  var x: KlassSendable? = KlassSendable()
  x = KlassSendable()
  let _ = { [weak x] in
    nonescapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

func testCopyableNonsendableClassWithNonescapingMainActorAsyncWeakCapture() {
  let x = KlassNonsendable()
  let _ = { [weak x] in
    nonescapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

func testMutableCopyableNonsendableClassWithNonescapingMainActorAsyncWeakCapture() {
  var x: KlassNonsendable? = KlassNonsendable()
  x = KlassNonsendable()
  let _ = { [weak x] in
    nonescapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

//////////////////////////////////
// MARK: Unowned Capture Lists //
//////////////////////////////////

func testCopyableSendableClassWithEscapingMainActorAsyncUnownedCapture() {
  let x = KlassSendable()
  let _ = { [unowned x] in
    escapingAsyncUse { @MainActor in
      useValue(x)
    }
  }
}

func testMutableCopyableSendableClassWithEscapingMainActorAsyncUnownedCapture() {
  var x = KlassSendable()
  x = KlassSendable()
  let _ = { [unowned x] in
    escapingAsyncUse { @MainActor in
      useValue(x)
    }
  }
}

func testCopyableNonsendableClassWithEscapingMainActorAsyncUnownedCapture() {
  let x = KlassNonsendable()
  let _ = { [unowned x] in
    escapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

func testMutableCopyableNonsendableClassWithEscapingMainActorAsyncUnownedCapture() {
  var x = KlassNonsendable()
  x = KlassNonsendable()
  let _ = { [unowned x] in
    escapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

func testCopyableSendableClassWithNonescapingMainActorAsyncUnownedCapture() {
  let x = KlassSendable()
  let _ = { [unowned x] in
    nonescapingAsyncUse { @MainActor in
      useValue(x)
    }
  }
}

func testMutableCopyableSendableClassWithNonescapingMainActorAsyncUnownedCapture() {
  var x = KlassSendable()
  x = KlassSendable()
  let _ = { [unowned x] in
    nonescapingAsyncUse { @MainActor in
      useValue(x)
    }
  }
}

func testCopyableNonsendableClassWithNonescapingMainActorAsyncUnownedCapture() {
  let x = KlassNonsendable()
  let _ = { [unowned x] in
    nonescapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

func testMutableCopyableNonsendableClassWithNonescapingMainActorAsyncUnownedCapture() {
  var x = KlassNonsendable()
  x = KlassNonsendable()
  let _ = { [unowned x] in
    nonescapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

/////////////////////////////////////////
// MARK: Generic Noncopyable Tests    //
/////////////////////////////////////////

func testGenericSendableWithEscapingMainActorAsync<T : ~Copyable>(_ value: consuming sending T,
                                                                  _ value2: consuming sending T) where T : Sendable {
  var x = value
  x = value2
  let _ = {
    escapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

func testMutableGenericSendableWithEscapingMainActorAsync<T : ~Copyable>(
  _ value: consuming sending T,
  _ value2: consuming sending T) where T : Sendable {
  var x = value
  x = value2
  let _ = {
    escapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

func testGenericNonsendableWithEscapingMainActorAsync<T : ~Copyable>(
  _ value: consuming sending T,
  _ value2: consuming sending T) {
  var x = value
  x = value2
  let _ = {
    escapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

func testMutableGenericNonsendableWithEscapingMainActorAsync<T : ~Copyable>(
  _ value: consuming sending T,
  _ value2: consuming sending T) {
  var x = value
  x = value2
  let _ = {
    escapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

func testGenericSendableWithNonescapingMainActorAsync<T : ~Copyable>(
  _ value: consuming sending T,
  _ value2: consuming sending T) where T : Sendable {
  var x = value
  x = value2
  let _ = {
    nonescapingAsyncUse { @MainActor in
      useValue(x)
    }
  }
}

func testMutableGenericSendableWithNonescapingMainActorAsync<T : ~Copyable>(
  _ value: consuming sending T,
  _ value2: consuming sending T) where T : Sendable {
  var x = value
  x = value2
  let _ = {
    nonescapingAsyncUse { @MainActor in
      useValue(x)
    }
  }
}

func testGenericNonsendableWithNonescapingMainActorAsync<T : ~Copyable>(
  _ value: consuming sending T,
  _ value2: consuming sending T) {
  var x = value
  x = value2
  let _ = {
    nonescapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}

func testMutableGenericNonsendableWithNonescapingMainActorAsync<T : ~Copyable>(
  _ value: consuming sending T,
  _ value2: consuming sending T) {
  var x = value
  x = value2
  let _ = {
    nonescapingAsyncUse { @MainActor in
      useValue(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}
