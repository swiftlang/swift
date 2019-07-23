/// Tests the fallback behavior for runtime library import paths. These should
/// prefer the resource directory, but fall back to the SDK.

// Assumption: We build the standard library with the compiler, so the default
// resource directory will contain a swiftmodule for the standard library.

// %t/good-sdk contains a loadable standard library.
// RUN: %empty-directory(%t/good-sdk)
// RUN: %empty-directory(%t/good-sdk/usr/lib/swift)
// RUN: cp -r %platform-module-dir/Swift.swiftmodule %t/good-sdk/usr/lib/swift/Swift.swiftmodule

// %t/bad-sdk contains an invalid standard library that cannot be loaded.
// RUN: %empty-directory(%t/bad-sdk)
// RUN: %empty-directory(%t/bad-sdk/usr/lib/swift/Swift.swiftmodule)
// RUN: touch %t/bad-sdk/usr/lib/swift/Swift.swiftmodule/garbage-garbage-garbage.swiftmodule

// %t/empty-toolchain does not contain a standard library.
// RUN: %empty-directory(%t/empty-toolchain)
// RUN: %empty-directory(%t/empty-toolchain/usr/lib/swift)

// FIXME: Until we have private imports, we need SwiftShims in the toolchain.
// RUN: cp -r %test-resource-dir/shims %t/empty-toolchain/usr/lib/swift/shims

// %t/really-empty-toolchain does not contain a standard library or SwiftShims.
// RUN: %empty-directory(%t/really-empty-toolchain)
// RUN: %empty-directory(%t/really-empty-toolchain/usr/lib/swift)

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
//
// We also check that ClangImporter noticed SwiftShims in the toolchain and
// didn't add a -isystem flag to look in the SDK.

// FIXME: We can't properly test this on a non-Darwin platform because we'll get
// the same error message for "unloadable standard library" and "no standard
// library". (SR-10097)
// REQUIRES: objc_interop

// RUN: %empty-directory(%t/mcp)
// RUN: not %target-swift-frontend(mock-sdk: -sdk %t/bad-sdk) -resource-dir %t/empty-toolchain/usr/lib/swift -module-cache-path %t/mcp -typecheck %s -dump-clang-diagnostics 2>&1 | %FileCheck --check-prefix CHECK-EMPTY %s
// CHECK-EMPTY-NOT: '-isystem' '{{.*}}/bad-sdk/usr/lib/swift/shims'
// CHECK-EMPTY: error: could not find module 'Swift' for target '{{.*}}'; found: garbage-garbage-garbage

// Check that, when the toolchain *doesn't* have SwiftShims in it, ClagImporter
// *does* add a -I flag to look in the SDK.

// RUN: %empty-directory(%t/mcp)
// RUN: not %target-swift-frontend(mock-sdk: -sdk %t/bad-sdk) -resource-dir %t/really-empty-toolchain/usr/lib/swift -module-cache-path %t/mcp -typecheck %s -dump-clang-diagnostics 2>&1 | %FileCheck --check-prefix CHECK-REALLY-EMPTY %s
// CHECK-REALLY-EMPTY: '-isystem' '{{.*}}/bad-sdk/usr/lib/swift/shims'
// CHECK-REALLY-EMPTY: error: could not find module 'Swift' for target '{{.*}}'; found: garbage-garbage-garbage

let x: Int = 1
