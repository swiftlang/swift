// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/MCP)

// This test makes sure that the module cache hash properly distinguishes
// between different targets, even when the path to the module interface is
// the same for multiple targets.

// 1. Build a .swiftinterface for a dummy module.

// RUN: %target-swift-frontend -typecheck -target x86_64-apple-macosx10.9 -emit-module-interface-path %t/SwiftModule.swiftinterface.tmp -parse-stdlib %s -module-name SwiftModule

// 2. Remove the -target line from the .swiftinterface. Clients will build using their target.

// RUN: sed -E 's/-target [^ ]+//g' %t/SwiftModule.swiftinterface.tmp > %t/SwiftModule.swiftinterface

// 3. Build for a bunch of different x86_64 targets, and ensure they all succeed by putting something else in the module cache.

// RUN: echo 'import SwiftModule' > %t/test.swift

// RUN: %target-swift-frontend -typecheck -sdk '' -target x86_64-apple-macosx10.9 -module-cache-path %t/MCP -parse-stdlib -I %t %t/test.swift
// RUN: %target-swift-frontend -typecheck -sdk '' -target x86_64-apple-tvos13.0   -module-cache-path %t/MCP -parse-stdlib -I %t %t/test.swift
// RUN: %target-swift-frontend -typecheck -sdk '' -target x86_64-apple-ios10.0 -module-cache-path %t/MCP -parse-stdlib -I %t %t/test.swift

// 4. Test iOS again, but with a newer minimum deployment target
// RUN: %target-swift-frontend -typecheck -sdk '' -target x86_64-apple-ios13.0    -module-cache-path %t/MCP -parse-stdlib -I %t %t/test.swift

// 5. Make sure there are only 3 .swiftmodules in the cache path (because iOS was reused)
// RUN: ls %t/MCP/*.swiftmodule | count 3
