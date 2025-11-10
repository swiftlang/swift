// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -o %t/test.swiftmodule -I %t -import-objc-header %t/bridging.h -enable-experimental-feature SafeInteropWrappers -strict-memory-safety -warnings-as-errors -Xcc -Werror -Xcc -Wno-nullability-completeness -Xcc -Wno-div-by-zero -Xcc -Wno-pointer-to-int-cast %t/test.swift -verify
// RUN: %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -o %t/test.swiftmodule -I %t -import-objc-header %t/bridging.h -enable-experimental-feature SafeInteropWrappers -strict-memory-safety -warnings-as-errors -Xcc -Werror -Xcc -Wno-nullability-completeness -Xcc -Wno-div-by-zero -Xcc -Wno-pointer-to-int-cast %t/test.swift -dump-macro-expansions 2>&1 | %FileCheck --dry-run > %t/macro-expansions.out
// RUN: %diff %t/macro-expansions.out %t/macro-expansions.expected
// RUN: %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -o %t/test.swiftmodule -I %t -import-objc-header %t/bridging.h -enable-experimental-feature SafeInteropWrappers -strict-memory-safety -warnings-as-errors -Xcc -Werror -Xcc -Wno-nullability-completeness -Xcc -Wno-div-by-zero -Xcc -Wno-pointer-to-int-cast %t/test.swift -dump-source-file-imports 2>&1 | %FileCheck --dry-run > %t/imports.out
// RUN: %diff %t/imports.out %t/imports.expected

//--- imports.expected
imports for TMP_DIR/test.swift:
	Swift
	__ObjC
	_StringProcessing
	_SwiftConcurrencyShims
	_Concurrency
imports for __ObjC.foo:
imports for @__swiftmacro_So3foo15_SwiftifyImportfMp_.swift:
	__ObjC
	Swift

//--- macro-expansions.expected
@__swiftmacro_So3foo15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func foo(_ p: Span<Int32>) {
    let len = Int32(exactly: p.count)!
    return unsafe p.withUnsafeBufferPointer { _pPtr in
      return unsafe foo(len, _pPtr.baseAddress!)
    }
}
------------------------------

//--- test.swift
func test(s: Span<CInt>) {
  foo(s)
}

//--- bridging.h
#include <ptrcheck.h>
#include <lifetimebound.h>

void foo(int len, const int * __counted_by(len) p __noescape);
