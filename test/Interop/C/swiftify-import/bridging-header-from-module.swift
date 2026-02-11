// REQUIRES: swift_feature_StabilizedSafeInteropWrappers

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -o %t/test.swiftmodule -I %t -import-objc-header %t/bridging.h -strict-memory-safety -warnings-as-errors -Xcc -Werror -Xcc -Wno-nullability-completeness -Xcc -Wno-div-by-zero -Xcc -Wno-pointer-to-int-cast %t/test.swift -verify
// RUN: %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -o %t/test.swiftmodule -I %t -import-objc-header %t/bridging.h -strict-memory-safety -warnings-as-errors -Xcc -Werror -Xcc -Wno-nullability-completeness -Xcc -Wno-div-by-zero -Xcc -Wno-pointer-to-int-cast %t/test.swift -dump-macro-expansions 2>&1 | %FileCheck --dry-run > %t/macro-expansions.out
// RUN: %diff %t/macro-expansions.out %t/macro-expansions.expected
// RUN: %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -o %t/test.swiftmodule -I %t -import-objc-header %t/bridging.h -strict-memory-safety -warnings-as-errors -Xcc -Werror -Xcc -Wno-nullability-completeness -Xcc -Wno-div-by-zero -Xcc -Wno-pointer-to-int-cast %t/test.swift -dump-source-file-imports 2>&1 | %FileCheck --dry-run > %t/imports.out
// RUN: %diff %t/imports.out %t/imports.expected

//--- imports.expected
imports for TMP_DIR/test.swift:
	Swift
	__ObjC
	_StringProcessing
	_SwiftConcurrencyShims
	_Concurrency
	A
imports for A.foo:
imports for @__swiftmacro_So3foo15_SwiftifyImportfMp_.swift:
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
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func foo(_ p: Span<Int32>, _ x: UnsafeMutablePointer<no_module_record_t>!) {
    let len = Int32(exactly: p.count)!
    let _pPtr = unsafe p.withUnsafeBufferPointer {
        unsafe $0
    }
    return unsafe foo(len, _pPtr.baseAddress!, x)
}
------------------------------

//--- test.swift
import A

func test(s: Span<a_t>, x: UnsafeMutablePointer<no_module_record_t>) {
  unsafe foo(s, x)
}

//--- bridging.h
// claim this header as part of bridging header, making it not belong to a module
#include "no-module.h"

//--- no-module.h
#pragma once
typedef int no_module_t;
struct no_module_record_t {
  int placeholder;
};

//--- a.h
#include <ptrcheck.h>
#include <lifetimebound.h>
// this module header now depends on a decl with no module
#include "no-module.h"
typedef int a_t;
void foo(int len, const int * __counted_by(len) p __noescape, struct no_module_record_t *x);

//--- module.modulemap
module A {
  header "a.h"
}
