
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -o %t/test.swiftmodule -I %t -import-objc-header %t/bridging.h -strict-memory-safety -warnings-as-errors -Xcc -Werror -Xcc -Wno-nullability-completeness -Xcc -Wno-div-by-zero -Xcc -Wno-pointer-to-int-cast %t/test.swift -verify \
// RUN:   -verify-additional-file %t%{fs-sep}bridging.h -verify-additional-file %t%{fs-sep}b.h -verify-additional-file %t%{fs-sep}c.h -Rclang-importer
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
	B
	C
imports for __ObjC.foo:
imports for @__swiftmacro_So3foo15_SwiftifyImportfMp_.swift:
	__ObjC
	Swift

//--- macro-expansions.expected
@__swiftmacro_So3foo15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func foo(_ p: Span<a_t>, _ x: UnsafeMutablePointer<no_module_record_t>!) {
    let len = no_module_t(exactly: p.count)!
    let _pPtr = unsafe p.withUnsafeBufferPointer {
        unsafe $0
    }
    defer {
        _fixLifetime(p)
    }
    return unsafe foo(len, _pPtr.baseAddress!, x)
}
------------------------------

//--- test.swift
import A
import B
import C

func test(s: Span<a_t>, x: UnsafeMutablePointer<no_module_record_t>) {
  unsafe foo(s, x)
}

func test2(p: UnsafeMutablePointer<CInt>, len: CInt, y: UnsafeMutablePointer<b_t>) {
  unsafe bar(len, p, y)
}

func test3(p: UnsafeMutablePointer<CInt>, len: CInt, z: UnsafeMutablePointer<c_t>) {
  unsafe baz(len, p, z)
}

//--- bridging.h
#include <ptrcheck.h>
#include <lifetimebound.h>

#include "no-module.h"
#include "a.h"
#include "c.h"

struct no_module_record_t;
// expected-remark@+1{{added safe interop wrapper}}
void foo(no_module_t len, const a_t * __counted_by(len) p __noescape, struct no_module_record_t *x);

struct b_t;
// expected-remark@+3{{did not add safe interop wrapper}}
// expected-note@+2{{clang function signature refers to concrete type without importing owning module}}
// expected-note@+1{{'bar' is in module __ObjC}}
void bar(int len, const int * __counted_by(len) p __noescape, struct b_t *y);

// expected-remark@+3{{did not add safe interop wrapper}}
// expected-note@+2{{clang function signature refers to concrete type without importing owning module}}
// expected-note@+1{{'baz' is in module __ObjC}}
void baz(int len, const int * __counted_by(len) p __noescape, struct c_t *z);

//--- no-module.h
typedef int no_module_t;
struct no_module_record_t {
  int placeholder;
};

//--- a.h
typedef int a_t;

//--- b.h
// expected-note@+1{{'b_t' is in module B}}
struct b_t {
  int placeholder;
};

//--- c.h
// expected-note@+1{{'c_t' is in module C}}
struct c_t {
  int placeholder;
};

//--- module.modulemap
module A {
  header "a.h"
}
module B {
  header "b.h"
}
module C {
  header "c.h"
}
