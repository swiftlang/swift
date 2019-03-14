/// Tests the fallback behavior for runtime library import paths. These should
/// prefer the resource directory, but fall back to the SDK.

// Assumption: We build the standard library with the compiler, so the default
// resource directory will contain a swiftmodule for the standard library.

// %t/good-sdk contains a loadable standard library.
// RUN: %empty-directory(%t/good-sdk/usr/lib/swift)
// RUN: cp -r %platform-module-dir/Swift.swiftmodule %t/good-sdk/usr/lib/swift/Swift.swiftmodule

// %t/bad-sdk contains an invalid standard library that cannot be loaded.
// RUN: %empty-directory(%t/bad-sdk/usr/lib/swift/Swift.swiftmodule)
// RUN: touch %t/bad-sdk/usr/lib/swift/Swift.swiftmodule/garbage-garbage-garbage.swiftmodule

// %t/empty-toolchain does not contain a standard library.
// RUN: %empty-directory(%t/empty-toolchain/usr/lib/swift)

// FIXME: Until we have private imports, we need SwiftShims in the toolchain.
// RUN: cp -r %test-resource-dir/shims %t/empty-toolchain/usr/lib/swift/shims

// If the compiler's resource directory does not contain a runtime swiftmodule,
// we should fall back to the SDK.

// RUN: %empty-directory(%t/mcp)
// RUN: %target-swift-frontend(mock-sdk: -sdk %t/good-sdk) -resource-dir %t/empty-toolchain/usr/lib/swift -module-cache-path %t/mcp -typecheck -verify %s

// If the compiler's resource directory *does* contain a runtime swiftmodule, we
// should *not* use the one in the SDK. (We assume that the resource directory
// built with this compiler does contain a standard library.)

// RUN: %empty-directory(%t/mcp)
// RUN: %target-swift-frontend(mock-sdk: -sdk %t/bad-sdk) -module-cache-path %t/mcp -typecheck -verify %s

// If neither the resource directory nor the SDK contains a runtime swiftmodule,
// loading should fail. This just proves that we aren't getting runtime imports
// some other way.

// FIXME: We can't properly test this on a non-Darwin platform because we'll get
// the same error message for "unloadable standard library" and "no standard
// library". (SR-10097)
// REQUIRES: objc_interop

// RUN: %empty-directory(%t/mcp)
// RUN: not %target-swift-frontend(mock-sdk: -sdk %t/bad-sdk) -resource-dir %t/empty-toolchain/usr/lib/swift -module-cache-path %t/mcp -typecheck %s 2>&1 | %FileCheck %s
// CHECK: error: could not find module 'Swift' for target '{{.*}}'; found: garbage-garbage-garbage

let x: Int = 1
