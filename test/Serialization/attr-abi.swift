// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// A module compiled in script mode wraps top-level variables in top-level code.
// RUN: %target-swift-frontend -emit-module %t/Script.swift -module-name Script \
// RUN:   -swift-version 6 -o %t/Script.swiftmodule
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t

//--- Script.swift

@abi(var abiStoredGlobal: Int)
public var storedGlobal: Int = 0

@abi(var abiComputedGlobal: Int)
public var computedGlobal: Int { 0 }

//--- Client.swift

import Script

@MainActor
func test() {
  storedGlobal = computedGlobal
}
