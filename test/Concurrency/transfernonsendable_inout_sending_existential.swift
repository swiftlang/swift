// RUN: %target-swift-frontend -emit-sil -swift-version 6 -disable-availability-checking -verify %s -o /dev/null

// Test that the region isolation checker correctly names values that are wrapped
// in an existential (any Protocol) before being stored into an indirect return
// slot.
//
// The SIL pattern is:
//   %concrete = <named value>
//   %exist = init_existential_ref %concrete : $Concrete : $Concrete, $any Protocol
//   %opt = enum $Optional<any Protocol>, #Optional.some!enumelt, %exist
//   store %opt to [init] %return_slot
//
// Previously, VariableNameInferrer::findDebugInfoProvidingValueHelper did not
// look through init_existential_ref, so the name walk would fail and the
// region isolation checker would fall through to the generic
// "unknown pattern" diagnostic instead of naming the value.

// REQUIRES: concurrency
// REQUIRES: synchronization

import Synchronization

// A simple non-Sendable class that satisfies a protocol.
protocol BoxProtocol: AnyObject {}

class Box: BoxProtocol {}

// MARK: - init_existential_ref look-through via Mutex.withLock

// The key regression test: returning a named concrete value through an
// existential wrapping (any BoxProtocol) while also storing it into the
// inout sending Mutex state.  The value flows through:
//   alloc_ref -> move_value [lexical] [var_decl] "item"
//             -> init_existential_ref
//             -> enum $Optional<any BoxProtocol>.some
//             -> store to indirect @out return slot
//
// Before the fix, init_existential_ref was not in the look-through list so
// name inference failed and the checker emitted the generic catch-all error.
// After the fix, the inferrer walks through init_existential_ref to the
// concrete value and finds the name "item".

let mutex = Mutex<Box?>(nil)

func getAsExistentialFromMutex() -> (any BoxProtocol)? {
    return mutex.withLock { state in
        let item = Box()
        state = item
        return item // expected-warning {{'item' cannot be returned}}
        // expected-note @-1 {{returning 'item' risks concurrent access to 'inout sending' parameter 'state' as caller assumes 'state' and result can be sent to different isolation domains}}
    }
}

// MARK: - Variant: explicit closure signature

func getAsExistentialExplicit() -> (any BoxProtocol)? {
    return mutex.withLock { (state: inout sending Box?) -> (any BoxProtocol)? in
        let item = Box()
        state = item
        return item // expected-warning {{'item' cannot be returned}}
        // expected-note @-1 {{returning 'item' risks concurrent access to 'inout sending' parameter 'state' as caller assumes 'state' and result can be sent to different isolation domains}}
    }
}

// MARK: - Safe pattern: returning a Sendable value is still OK

func getSendableFromMutex() -> String {
    return mutex.withLock { (state: inout sending Box?) -> String in
        let item = Box()
        state = item
        return "ok"  // OK: String is Sendable, no region violation
    }
}
