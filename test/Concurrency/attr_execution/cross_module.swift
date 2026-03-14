// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

/// Build the library A
// RUN: %target-swift-frontend -emit-module %t/src/A.swift \
// RUN:   -module-name A -swift-version 6 -enable-library-evolution \
// RUN:   -emit-module-path %t/A.swiftmodule \
// RUN:   -emit-module-interface-path %t/A.swiftinterface

// Build the client using module
// RUN: %target-swift-emit-sil -verify -module-name Client -I %t %t/src/Client.swift

// RUN: rm %t/A.swiftmodule

// Re-build the client using interface
// RUN: %target-swift-emit-sil -verify -module-name Client -I %t %t/src/Client.swift

//--- A.swift
@MainActor
public final class Test {
  public func test(_: @escaping @Sendable @MainActor () -> Void) {}
}

//--- Client.swift
import A

@MainActor
func test(t: Test, fn: @escaping @Sendable @MainActor () -> Void) {
  t.test(fn) // Ok
}
