
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -o %t/test.swiftmodule -I %t -import-bridging-header %t/bridging.h -strict-memory-safety %t/test.swift \
// RUN:   -verify

// RUN: %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -o %t/test.swiftmodule -I %t -import-bridging-header %t/bridging.h -strict-memory-safety %t/test.swift \
// RUN:   -dump-macro-expansions 2>&1 | %FileCheck --dry-run > %t/macro-expansions.out
// RUN: %diff %t/macro-expansions.out %t/macro-expansions.expected

// RUN: %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -o %t/test.swiftmodule -I %t -import-bridging-header %t/bridging.h -strict-memory-safety %t/test.swift \
// RUN:   -dump-source-file-imports 2>&1 | %FileCheck --dry-run > %t/imports.out
// RUN: %diff %t/imports.out %t/imports.expected

// This is a regression test that previously triggered `ASSERT(OwningModule || IsInBridgingHeader)`
// in swiftifyImpl. It makes sure that the logic for fetching the owning module alignes with the
// logic in getClangModuleForDecl().

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
	ptrcheck
	_StringProcessing
	_SwiftConcurrencyShims
	_Concurrency

//--- macro-expansions.expected
@__swiftmacro_So3foo15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func foo(_ p: UnsafeMutableBufferPointer<Int32>) {
    let len = Int32(exactly: p.count)!
    return unsafe foo(len, p.baseAddress!)
}
------------------------------

//--- test.swift
import A

func test(p: UnsafeMutableBufferPointer<CInt>) {
  unsafe foo(p)
}

//--- bridging.h
#include <ptrcheck.h>
#include <a.h>

void foo(int len, int * __counted_by(len) p);

//--- a.h
#include <ptrcheck.h>

void foo(int len, int * __counted_by(len) p);

//--- module.modulemap
module A {
  header "a.h"
}
