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
  let x = NoncopyableStructNonsendable() // expected-error {{sending 'x' risks causing data races}}
  // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
  let _ = {
    nonescapingAsyncUse { @MainActor in
      useValue(x)
    }
  }
}

func testMutableNoncopyableNonsendableStructWithNonescapingMainActorAsync() {
  var x = NoncopyableStructNonsendable() // expected-error {{sending 'x' risks causing data races}}
  // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
  x = NoncopyableStructNonsendable()
  let _ = {
    nonescapingAsyncUse { @MainActor in
      useValue(x)
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
  let _ = { [x] in // expected-error {{sending 'x' risks causing data races}}
    // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    nonescapingAsyncUse { @MainActor in
      useValue(x)
    }
  }
}

func testMutableNoncopyableNonsendableStructWithNonescapingMainActorAsyncNormalCapture() {
  var x = NoncopyableStructNonsendable()
  x = NoncopyableStructNonsendable()
  let _ = { [x] in // expected-error {{sending 'x' risks causing data races}}
    // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    nonescapingAsyncUse { @MainActor in
      useValue(x)
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
      useValue(x)
    }
  }
}

func testMutableCopyableSendableClassWithEscapingMainActorAsyncWeakCapture() {
  var x: KlassSendable? = KlassSendable()
  x = KlassSendable()
  let _ = { [weak x] in
    escapingAsyncUse { @MainActor in
      useValue(x)
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
      useValue(x)
    }
  }
}

func testMutableCopyableSendableClassWithNonescapingMainActorAsyncWeakCapture() {
  var x: KlassSendable? = KlassSendable()
  x = KlassSendable()
  let _ = { [weak x] in
    nonescapingAsyncUse { @MainActor in
      useValue(x)
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

////////////////////////////////////////////
// MARK: Advanced Weak Capture Patterns  //
////////////////////////////////////////////

// Test: Chained closures with weak captures (single weak reference passed through chain)
// This exercises the ConvertWeakVarCaptureToWeakLet optimization for multi-level closures
func testChainedClosuresReadOnlyWeakCaptureSendable() {
  let obj = KlassSendable()

  // Outer closure captures obj weakly
  let outer = { [weak obj] in
    // Inner closure also captures the weak reference
    let inner = {
      escapingAsyncUse { @MainActor in
        // Only reading from weak capture - should be promotable to 'let'
        if let obj = obj {
          useValue(obj)
        }
      }
    }
    inner()
  }
  outer()
}

func testChainedClosuresReadOnlyWeakCaptureNonsendable() {
  let obj = KlassNonsendable()

  let outer = { [weak obj] in
    let inner = {
      escapingAsyncUse { @MainActor in
        if let obj = obj { // expected-error {{sending 'obj' risks causing data races}}
                  // expected-note @-1 {{task-isolated 'obj' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
          useValue(obj)
        }
      }
    }
    inner()
  }
  outer()
}

// Test: Multiple weak captures in same closure
// Verifies that optimization handles multiple weak boxes independently
func testMultipleWeakCapturesSendable() {
  let obj1 = KlassSendable()
  let obj2 = KlassSendable()
  let obj3 = KlassSendable()

  let _ = { [weak obj1, weak obj2, weak obj3] in
    escapingAsyncUse { @MainActor in
      // All three are read-only - all should be promotable
      if let o1 = obj1 { useValue(o1) }
      if let o2 = obj2 { useValue(o2) }
      if let o3 = obj3 { useValue(o3) }
    }
  }
}

func testMultipleWeakCapturesNonsendable() {
  let obj1 = KlassNonsendable()
  let obj2 = KlassNonsendable()
  let obj3 = KlassNonsendable()

  let _ = { [weak obj1, weak obj2, weak obj3] in
    escapingAsyncUse { @MainActor in
      if let o1 = obj1 { // expected-error {{sending 'obj1' risks causing data races}}
        useValue(o1) // expected-note @-1 {{task-isolated 'obj1' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
      }
      if let o2 = obj2 { // expected-error {{sending 'obj2' risks causing data races}}
        // expected-note @-1 {{task-isolated 'obj2' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
        useValue(o2)
      }
      if let o3 = obj3 { // expected-error {{sending 'obj3' risks causing data races}}
        // expected-note @-1 {{task-isolated 'obj3' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
        useValue(o3)
      }
    }
  }
}

// In this case, even though we are writing to the outside mutableObj, the inner
// mutableObj is actually a separate capture variable that shadows
// mutableObj. So we perform the transformation on that.
func testMixedWeakCapturesReadAndWriteSendable() {
  var mutableObj: KlassSendable? = KlassSendable()
  let immutableObj = KlassSendable()

  let _ = { [weak mutableObj, weak immutableObj] in
    escapingAsyncUse { @MainActor in
      // Read from immutable (should promote to let)
      if let obj = immutableObj {
        useValue(obj)
      }

      // Read from immutable. The binding here is just shadowing the outside
      // mutable object.
      if let obj = mutableObj {
        useValue(obj)
      }
    }
  }

  mutableObj = nil
}

// Test: Weak self pattern (common in delegate/callback scenarios)
class DelegateSendable: @unchecked Sendable {
  func setupCallback() {
    let _ = { [weak self] in
      escapingAsyncUse { @MainActor in
        // Common pattern: weak self to avoid retain cycles
        guard let self = self else { return }
        useValue(self)
      }
    }
  }
}

class DelegateNonsendable {
  func setupCallback() {
    let _ = { [weak self] in
      escapingAsyncUse { @MainActor in
        guard let self = self else { return } // expected-error {{sending 'self' risks causing data races}}
        // expected-note @-1 {{task-isolated 'self' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
        useValue(self)
      }
    }
  }
}

// Test: Weak capture with control flow (loops)
func testWeakCaptureInLoopSendable() {
  let obj = KlassSendable()

  let _ = { [weak obj] in
    escapingAsyncUse { @MainActor in
      // Accessing weak capture multiple times in a loop
      for _ in 0..<10 {
        if let obj = obj {
          useValue(obj)
        }
      }
    }
  }
}

func testWeakCaptureInLoopNonsendable() {
  let obj = KlassNonsendable()

  let _ = { [weak obj] in
    escapingAsyncUse { @MainActor in
      for _ in 0..<10 {
        if let obj = obj { // expected-error {{sending 'obj' risks causing data races}}
          // expected-note @-1 {{task-isolated 'obj' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
          useValue(obj)
        }
      }
    }
  }
}

// Test: Weak capture with conditional access
func testWeakCaptureConditionalAccessSendable() {
  let obj = KlassSendable()
  let condition = true

  let _ = { [weak obj] in
    escapingAsyncUse { @MainActor in
      if condition {
        if let obj = obj {
          useValue(obj)
        }
      } else {
        // Not accessing weak capture in this branch
      }
    }
  }
}

// Test: Nested weak captures (closure within closure, both capture weakly)
func testNestedWeakCapturesSendable() {
  let outer = KlassSendable()

  let _ = { [weak outer] in
    let inner = KlassSendable()

    // TODO: We should be able to handle multiple.
    let _ = { [weak outer, weak inner] in
      escapingAsyncUse { @MainActor in
        if let o = outer { useValue(o) }
        if let i = inner { useValue(i) }
      }
    }
  }
}

func testNestedWeakCapturesNonsendable() {
  let outer = KlassNonsendable()

  let _ = { [weak outer] in
    let inner = KlassNonsendable()

    let _ = { [weak outer, weak inner] in
      escapingAsyncUse { @MainActor in
        if let o = outer { // expected-error {{sending 'outer' risks causing data races}}
          // expected-note @-1 {{task-isolated 'outer' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
          useValue(o)
        }
        if let i = inner { // expected-error {{sending 'inner' risks causing data races}}
          // expected-note @-1 {{task-isolated 'inner' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
          useValue(i)
        }
      }
    }
  }
}

// Test: Escaping closure that returns weak reference
func testEscapingClosureReturningWeakCaptureSendable() -> (() -> KlassSendable?) {
  let obj = KlassSendable()

  // Closure escapes and holds weak reference
  return { [weak obj] in
    return obj
  }
}

func testEscapingClosureReturningWeakCaptureNonsendable() -> (() -> KlassNonsendable?) {
  let obj = KlassNonsendable()

  return { [weak obj] in
    return obj
  }
}

// Test: Weak capture with immediate deallocation (edge case)
func testWeakCaptureImmediateDeallocSendable() {
  do {
    let obj = KlassSendable()
    let _ = { [weak obj] in
      escapingAsyncUse { @MainActor in
        // obj might already be deallocated
        if let obj = obj {
          useValue(obj)
        }
      }
    }
    // obj deallocates here
  }
}

let globalSendable = KlassSendable()

// Test: Chained closures where intermediate closure modifies different captures
// This matches the SIL test pattern: callee_1 writes to one box, callee_2 reads all
func testChainedClosuresPartialWriteSendable() {
  let readOnly1 = KlassSendable()
  var writable = KlassSendable()
  writable = KlassSendable()
  let readOnly2 = KlassSendable()

  let _ = { [weak readOnly1, weak writable, weak readOnly2] in
    // First level: modify writable, read others
    if let obj = readOnly1 { useValue(obj) }
    writable = globalSendable  // Modify this one
    if let obj = readOnly2 { useValue(obj) }

    // Second level: read all including the modified one
    let _ = {
      escapingAsyncUse { @MainActor in
        if let obj = readOnly1 { useValue(obj) }  // Read-only throughout
        if let obj = writable { // expected-error {{sending 'writable' risks causing data races}}
          // expected-note @-1 {{task-isolated 'writable' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
          useValue(obj)
        }
        if let obj = readOnly2 { useValue(obj) }  // Read-only throughout
      }
    }
  }
}

// Test: Weak capture list with transformation
func testWeakCaptureWithTransformationSendable() {
  let obj = KlassSendable()

  let _ = { [weak obj] in
    // Transform weak optional to strong optional
    let strongRef = obj

    escapingAsyncUse { @MainActor in
      if let strong = strongRef {
        useValue(strong)
      }
    }
  }
}

// Test: Multiple closures capturing same weak reference (but separately)
func testMultipleClosuresSameWeakCaptureSendable() {
  let obj = KlassSendable()

  let closure1 = { [weak obj] in
    escapingAsyncUse { @MainActor in
      if let o = obj { useValue(o) }
    }
  }

  let closure2 = { [weak obj] in
    escapingAsyncUse { @MainActor in
      if let o = obj { useValue(o) }
    }
  }

  closure1()
  closure2()
}

func testMultipleClosuresSameWeakCaptureNonsendable() {
  let obj = KlassNonsendable()

  let closure1 = { [weak obj] in
    escapingAsyncUse { @MainActor in
      if let o = obj { // expected-error {{sending 'obj' risks causing data races}}
        // expected-note @-1 {{task-isolated 'obj' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
        useValue(o)
      }
    }
  }

  let closure2 = { [weak obj] in
    escapingAsyncUse { @MainActor in
      if let o = obj { // expected-error {{sending 'obj' risks causing data races}}
        // expected-note @-1 {{task-isolated 'obj' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
        useValue(o)
      }
    }
  }

  closure1()
  closure2()
}

// Test: Empty closure with weak capture (should still optimize)
func testEmptyClosureWeakCaptureSendable() {
  let obj = KlassSendable()

  let _ = { [weak obj] in
    escapingAsyncUse { @MainActor in
      // Capture exists but unused - still promotable to let
      _ = obj
    }
  }
}

///////////////////////////////////////////////////
// MARK: Protocol-Based Weak Capture Tests       //
// (Tests with class-bound protocol constraints) //
///////////////////////////////////////////////////

// Protocol declarations for testing
protocol ProtocolSendable: AnyObject, Sendable {
  func doSomething()
}

protocol ProtocolNonsendable: AnyObject {
  func doSomething()
}

// Test: Simple weak capture of protocol type (Sendable)
func testWeakCaptureProtocolSendable<T: ProtocolSendable>(_ obj: T) {
  let _ = { [weak obj] in
    escapingAsyncUse { @MainActor in
      if let obj = obj {
        useValue(obj)
      }
    }
  }
}

// Test: Simple weak capture of protocol type (Nonsendable)
func testWeakCaptureProtocolNonsendable<T: ProtocolNonsendable>(_ obj: T) {
  let _ = { [weak obj] in
    escapingAsyncUse { @MainActor in
      if let obj = obj { // expected-error {{sending 'obj' risks causing data races}}
        // expected-note @-1 {{task-isolated 'obj' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
        useValue(obj)
      }
    }
  }
}

// Test: Chained closures with protocol weak captures
func testChainedClosuresProtocolSendable<T: ProtocolSendable>(_ obj: T) {
  let outer = { [weak obj] in
    let inner = {
      escapingAsyncUse { @MainActor in
        if let obj = obj {
          useValue(obj)
        }
      }
    }
    inner()
  }
  outer()
}

func testChainedClosuresProtocolNonsendable<T: ProtocolNonsendable>(_ obj: T) {
  let outer = { [weak obj] in
    let inner = {
      escapingAsyncUse { @MainActor in
        if let obj = obj { // expected-error {{sending 'obj' risks causing data races}}
          // expected-note @-1 {{task-isolated 'obj' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
          useValue(obj)
        }
      }
    }
    inner()
  }
  outer()
}

// Test: Multiple protocol weak captures
func testMultipleProtocolWeakCapturesSendable<T: ProtocolSendable, U: ProtocolSendable, V: ProtocolSendable>(_ obj1: T, _ obj2: U, _ obj3: V) {
  let _ = { [weak obj1, weak obj2, weak obj3] in
    escapingAsyncUse { @MainActor in
      if let o1 = obj1 { useValue(o1) }
      if let o2 = obj2 { useValue(o2) }
      if let o3 = obj3 { useValue(o3) }
    }
  }
}

func testMultipleProtocolWeakCapturesNonsendable<T: ProtocolNonsendable, U: ProtocolNonsendable, V: ProtocolNonsendable>(_ obj1: T, _ obj2: U, _ obj3: V) {
  let _ = { [weak obj1, weak obj2, weak obj3] in
    escapingAsyncUse { @MainActor in
      if let o1 = obj1 { // expected-error {{sending 'obj1' risks causing data races}}
        // expected-note @-1 {{task-isolated 'obj1' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
        useValue(o1)
      }
      if let o2 = obj2 { // expected-error {{sending 'obj2' risks causing data races}}
        // expected-note @-1 {{task-isolated 'obj2' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
        useValue(o2)
      }
      if let o3 = obj3 { // expected-error {{sending 'obj3' risks causing data races}}
        // expected-note @-1 {{task-isolated 'obj3' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
        useValue(o3)
      }
    }
  }
}

// Test: Protocol conforming to itself with weak self pattern
protocol DelegateProtocolSendable: AnyObject, Sendable {
  func setupCallback()
}

protocol DelegateProtocolNonsendable: AnyObject {
  func setupCallback()
}

class ConcreteDelegateSendable: DelegateProtocolSendable, @unchecked Sendable {
  func setupCallback() {
    let _ = { [weak self] in
      escapingAsyncUse { @MainActor in
        guard let self = self else { return }
        useValue(self)
      }
    }
  }
}

class ConcreteDelegateNonsendable: DelegateProtocolNonsendable {
  func setupCallback() {
    let _ = { [weak self] in
      escapingAsyncUse { @MainActor in
        guard let self = self else { return } // expected-error {{sending 'self' risks causing data races}}
        // expected-note @-1 {{task-isolated 'self' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
        useValue(self)
      }
    }
  }
}

// Test: Nested weak captures with protocols
func testNestedProtocolWeakCapturesSendable<T: ProtocolSendable, U: ProtocolSendable>(_ outer: T, _ inner: U) {
  let _ = { [weak outer] in
    let _ = { [weak outer, weak inner] in
      escapingAsyncUse { @MainActor in
        if let o = outer { useValue(o) }
        if let i = inner { useValue(i) }
      }
    }
  }
}

func testNestedProtocolWeakCapturesNonsendable<T: ProtocolNonsendable, U: ProtocolNonsendable>(_ outer: T, _ inner: U) {
  let _ = { [weak outer] in
    let _ = { [weak outer, weak inner] in
      escapingAsyncUse { @MainActor in
        if let o = outer { // expected-error {{sending 'outer' risks causing data races}}
          // expected-note @-1 {{task-isolated 'outer' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
          useValue(o)
        }
        if let i = inner { // expected-error {{sending 'inner' risks causing data races}}
          // expected-note @-1 {{task-isolated 'inner' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
          useValue(i)
        }
      }
    }
  }
}

// Test: Protocol with associated type constraints (more complex generic case)
protocol DataSourceProtocol: AnyObject, Sendable {
  associatedtype Item
  func fetchItem() -> Item
}

class ConcreteDataSource<T>: DataSourceProtocol, @unchecked Sendable {
  func fetchItem() -> T {
    fatalError()
  }
}

func testWeakCaptureProtocolWithAssociatedTypeSendable<DS: DataSourceProtocol>(_ dataSource: DS) {
  let _ = { [weak dataSource] in
    escapingAsyncUse { @MainActor in
      if let ds = dataSource {
        useValue(ds)
      }
    }
  }
}

// Test: Weak capture in protocol extension
extension ProtocolSendable {
  func createWeakCapturingClosure() -> () -> Void {
    return { [weak self] in
      escapingAsyncUse { @MainActor in
        guard let self = self else { return }
        useValue(self)
      }
    }
  }
}

extension ProtocolNonsendable {
  func createWeakCapturingClosure() -> () -> Void {
    return { [weak self] in
      escapingAsyncUse { @MainActor in
        guard let self = self else { return } // expected-error {{sending 'self' risks causing data races}}
        // expected-note @-1 {{task-isolated 'self' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
        useValue(self)
      }
    }
  }
}

// Test: Existential vs generic - both should work
func testExistentialWeakCaptureSendable(_ obj: any ProtocolSendable) {
  let _ = { [weak obj] in
    escapingAsyncUse { @MainActor in
      if let obj = obj {
        useValue(obj)
      }
    }
  }
}

func testExistentialWeakCaptureNonsendable(_ obj: any ProtocolNonsendable) {
  let _ = { [weak obj] in
    escapingAsyncUse { @MainActor in
      if let obj = obj { // expected-error {{sending 'obj' risks causing data races}}
        // expected-note @-1 {{task-isolated 'obj' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
        useValue(obj)
      }
    }
  }
}

// Test: Mixed protocol and class weak captures
func testMixedProtocolClassWeakCapturesSendable<T: ProtocolSendable>(_ protocolObj: T, _ classObj: KlassSendable) {
  let _ = { [weak protocolObj, weak classObj] in
    escapingAsyncUse { @MainActor in
      if let p = protocolObj { useValue(p) }
      if let c = classObj { useValue(c) }
    }
  }
}

// Test: Protocol composition with weak capture
func testProtocolCompositionWeakCapture(_ obj: any (ProtocolSendable & Sendable)) {
  let _ = { [weak obj] in
    escapingAsyncUse { @MainActor in
      if let obj = obj {
        useValue(obj)
      }
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
  var x = value // expected-error {{sending 'x' risks causing data races}}
  // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
  x = value2
  let _ = {
    nonescapingAsyncUse { @MainActor in
      useValue(x)
    }
  }
}

func testMutableGenericNonsendableWithNonescapingMainActorAsync<T : ~Copyable>(
  _ value: consuming sending T,
  _ value2: consuming sending T) {
  var x = value // expected-error {{sending 'x' risks causing data races}}
  // expected-note @-1 {{task-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
  x = value2
  let _ = {
    nonescapingAsyncUse { @MainActor in
      useValue(x)
    }
  }
}
