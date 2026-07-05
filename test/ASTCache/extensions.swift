// RUN: rm -rf %t.cache
// RUN: mkdir -p %t.cache
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -parse-as-library -emit-object -o %t.cold.o %s 2>&1 | %FileCheck %s --check-prefix=COLD
// RUN: %target-swift-frontend -experimental-ast-cache %t.cache -debug-ast-cache \
// RUN:   -module-name testmod -parse-as-library -emit-object -o %t.warm.o %s 2>&1 | %FileCheck %s --check-prefix=WARM

// COLD: AST cache: MISS (no cache file)
// COLD: AST cache: SAVED

// WARM: AST cache: HIT

// Test that extension binding works with AST cache HIT.
// Deserialized extensions already have ExtendedNominal set; this test verifies
// that bindExtensions() does not corrupt the binding (which previously caused
// "circular reference" errors during warm/incremental builds).
// The extension method must compile without circular reference errors.

// --- Test 1: struct with extension method + operator in extension ---
struct Container {
  var value: Int

  func doubled() -> Int {
    return value * 2
  }
}

extension Container: Equatable {
  func tripled() -> Int {
    return value * 3
  }

  func sum(with other: Container) -> Int {
    return value + other.value
  }

  // Operator defined inside an extension — verifies that operators in
  // extensions of cached files are still resolvable in warm builds.
  static func == (lhs: Container, rhs: Container) -> Bool {
    return lhs.value == rhs.value
  }
}

// --- Test 2: ~Copyable type with extension ---
// Verifies that Copyable synthesis gating works for deserialized decls.
struct Resource: ~Copyable {
  var handle: Int

  func isValid() -> Bool {
    return handle != 0
  }
}

extension Resource {
  func describe() -> String {
    return "Resource(\(handle))"
  }
}
