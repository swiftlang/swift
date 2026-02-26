// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -import-objc-header %S/Inputs/non_runtime_protocol.h %s

// REQUIRES: objc_interop

import Foundation

// === ALLOWED CASES ===
// Using non-runtime protocol in function signatures should be allowed
// These usages don't require runtime protocol metadata

@objc class AllowedUsages : NSObject {
  // Using protocol in array type - allowed (no metadata needed for static type)
  @objc public func runMutations(_ mutations: [NonRuntimeBridging]) {
    for _ in mutations {
    }
  }

  // Using protocol as parameter type - allowed
  @objc public func processMutation(_ mutation: NonRuntimeBridging) {
  }

  // Using protocol as return type - allowed
  @objc public func getMutation() -> NonRuntimeBridging? {
    return nil
  }
}

// Conforming to non-runtime protocol should be allowed
class MyConformer : NonRuntimeBridging {
}

// Using protocol as generic constraint - allowed
func genericFunc<T: NonRuntimeBridging>(_ value: T) {
}

// === DISALLOWED CASES ===
// Using non-runtime protocol in contexts requiring runtime metadata should error

func testDynamicCasts(_ obj: AnyObject) {
  // Dynamic cast to non-runtime protocol - should error
  let _ = obj as? NonRuntimeBridging // expected-error {{protocol 'NonRuntimeBridging' has the 'objc_non_runtime_protocol' attribute and cannot be used in a context that requires runtime protocol metadata}}

  let _ = obj is NonRuntimeBridging // expected-error {{protocol 'NonRuntimeBridging' has the 'objc_non_runtime_protocol' attribute and cannot be used in a context that requires runtime protocol metadata}}

  let _ = obj as! NonRuntimeBridging // expected-error {{protocol 'NonRuntimeBridging' has the 'objc_non_runtime_protocol' attribute and cannot be used in a context that requires runtime protocol metadata}}
}

// Test protocol composition with non-runtime protocol
func testProtocolCompositionCasts(_ obj: AnyObject) {
  // Composition containing non-runtime protocol should error
  let _ = obj as? (NonRuntimeBridging & RegularProtocol) // expected-error {{protocol 'NonRuntimeBridging' has the 'objc_non_runtime_protocol' attribute and cannot be used in a context that requires runtime protocol metadata}}

  let _ = obj is (NonRuntimeBridging & RegularProtocol) // expected-error {{protocol 'NonRuntimeBridging' has the 'objc_non_runtime_protocol' attribute and cannot be used in a context that requires runtime protocol metadata}}
}

// Test with multiple non-runtime protocols in composition
func testMultipleNonRuntimeProtocols(_ obj: AnyObject) {
  // Note: The order of protocols in composition may vary, so we check for either
  let _ = obj as? (NonRuntimeBridging & AnotherNonRuntimeProtocol) // expected-error {{protocol 'AnotherNonRuntimeProtocol' has the 'objc_non_runtime_protocol' attribute and cannot be used in a context that requires runtime protocol metadata}}
}

// Regular protocols should still work fine with dynamic casts
func testRegularProtocolCasts(_ obj: AnyObject) {
  let _ = obj as? RegularProtocol
  let _ = obj is RegularProtocol
  let _ = obj as! RegularProtocol
}
