
// This test looks at the behavior of how ossa modules deal with the prebuilt
// module cache. We compile a fake swift stdlib with a module path and a swift
// interface file.
//
// Then we test the behavior of the importing mechanism and make sure that no
// matter whether or not we compile the library with OSSA that

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/TempModuleCacheLibrary)
// RUN: %empty-directory(%t/TempModuleCacheBuilder)
// RUN: %empty-directory(%t/TempModuleCacheOther)
// RUN: %empty-directory(%t/SDK/usr/lib/swift/%relative-platform-module-dir-prefix/Swift.swiftmodule)
// RUN: %empty-directory(%t/PreBuiltSDKModules)

// RUN: %target-swift-frontend -c -emit-module-interface-path %t/SDK/usr/lib/swift/%relative-platform-module-dir-prefix/Swift.swiftmodule/%target-swiftinterface-name -emit-module-path %t/SDK/usr/lib/swift/%relative-platform-module-dir-prefix/Swift.swiftmodule/%target-swiftmodule-name -o %t/Swift.o -parse-stdlib -module-name Swift -enable-library-evolution -module-cache-path %t/TempModuleCacheLibrary -swift-version 5 %s -disable-objc-interop

// RUN: %swift_build_sdk_interfaces_base -o %t/PreBuiltSDKModules -j 1 -sdk %t/SDK -module-cache-path %t/TempModuleCacheBuilder %t/SDK -v

// In this case, we should not rebuild.

// RUN: %target-swift-frontend -typecheck -sdk '%t/SDK' -prebuilt-module-cache-path '%t/PreBuiltSDKModules' -module-cache-path %t/TempModuleCacheOther -resource-dir '' -parse-stdlib -Rmodule-interface-rebuild %S/Inputs/sdk-test-stdlib-no-ossa-referent-no-rebuild-remark.swift -verify

// In this case also, we should not rebuild.

// RUN: %target-swift-frontend -typecheck -sdk '%t/SDK' -prebuilt-module-cache-path '%t/PreBuiltSDKModules' -module-cache-path %t/TempModuleCacheOther -resource-dir '' -parse-stdlib -Rmodule-interface-rebuild %S/Inputs/sdk-test-stdlib-no-ossa-referent-no-rebuild-remark.swift -verify

// Flaky hangs: rdar://77288690
// UNSUPPORTED: CPU=arm64, CPU=arm64e

@_fixed_layout
public final class Klass {
  public init() {
  }
}

@inlinable
public func foo() -> Klass {
  return Klass()
}



