// REQUIRES: swift_feature_StabilizedSafeInteropWrappers

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swiftc_driver -typecheck -disable-bridging-pch -plugin-path %swift-plugin-dir -o %t/test.swiftmodule -I %t -Xcc -I -Xcc %t -import-objc-header %t/bridging.h -strict-memory-safety -warnings-as-errors -Xcc -Werror -Xcc -Wno-nullability-completeness -Xcc -Wno-div-by-zero -Xcc -Wno-pointer-to-int-cast %t/test.swift -Xfrontend -verify
// RUN: %target-swiftc_driver -typecheck -disable-bridging-pch -plugin-path %swift-plugin-dir -o %t/test.swiftmodule -I %t -import-objc-header %t/bridging.h -strict-memory-safety -warnings-as-errors -Xcc -Werror -Xcc -Wno-nullability-completeness -Xcc -Wno-div-by-zero -Xcc -Wno-pointer-to-int-cast %t/test.swift -Xfrontend -dump-macro-expansions 2>&1 | %FileCheck --dry-run > %t/macro-expansions.out
// RUN: %diff %t/macro-expansions.out %t/macro-expansions.expected
// RUN: %target-swiftc_driver -typecheck -disable-bridging-pch -plugin-path %swift-plugin-dir -o %t/test.swiftmodule -I %t -import-objc-header %t/bridging.h -strict-memory-safety -warnings-as-errors -Xcc -Werror -Xcc -Wno-nullability-completeness -Xcc -Wno-div-by-zero -Xcc -Wno-pointer-to-int-cast %t/test.swift -Xfrontend -dump-source-file-imports 2>&1 | %FileCheck --dry-run > %t/imports.out
// RUN: %diff %t/imports.out %t/imports.expected

//--- imports.expected
imports for TMP_DIR/test.swift:
	Swift
	__ObjC
	_StringProcessing
	_SwiftConcurrencyShims
	_Concurrency
	TestClang
imports for __ObjC.foo:
imports for @__swiftmacro_So3foo15_SwiftifyImportfMp_.swift:
	__ObjC
	Swift
imports for TestClang.bar:
imports for @__swiftmacro_So3bar15_SwiftifyImportfMp_.swift:
	Swift
	lifetimebound
	ptrcheck
	_StringProcessing
	_SwiftConcurrencyShims
	_Concurrency

//--- macro-expansions.expected
@__swiftmacro_So3foo15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func foo(_ p: Span<Int32>) {
    let len = Int32(exactly: p.count)!
    let _pPtr = unsafe p.withUnsafeBufferPointer {
        unsafe $0
    }
    return unsafe foo(len, _pPtr.baseAddress!)
}
------------------------------
@__swiftmacro_So3bar15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func bar(_ p: Span<Int32>) {
    let len = Int32(exactly: p.count)!
    let _pPtr = unsafe p.withUnsafeBufferPointer {
        unsafe $0
    }
    return unsafe bar(len, _pPtr.baseAddress!)
}
------------------------------

//--- test.swift
import TestClang

func test(s: Span<CInt>) {
  foo(s)
  bar(s)
}

//--- test2.swift
import TestClang

func test2(s: Span<CInt>) {
  foo(s)
  bar(s)
}

//--- bridging.h
#include <ptrcheck.h>
#include <lifetimebound.h>

void foo(int len, const int * __counted_by(len) p __noescape);

static const int placeholder = 2;

//--- test-clang.h
#include <ptrcheck.h>
#include <lifetimebound.h>

void bar(int len, const int * __counted_by(len) p __noescape);

//--- module.modulemap
module TestClang {
  header "test-clang.h"
}

