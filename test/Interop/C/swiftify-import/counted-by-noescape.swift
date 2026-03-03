// REQUIRES: swift_feature_Lifetimes

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -enable-experimental-feature Lifetimes -strict-memory-safety -Xcc -Wno-ignored-attributes -Xcc -Wno-nullability-completeness \
// RUN:   %t/test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -suppress-notes

// Check that ClangImporter correctly infers and expands @_SwiftifyImport macros for functions with __counted_by __noescape parameters.

//--- test.h
#pragma once

#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __noescape __attribute__((noescape))

// expected-expansion@+13:58{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public func simple(_ p: inout MutableSpan<Int32>) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = unsafe p.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe simple(len, _pPtr.baseAddress!)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void simple(int len, int * __counted_by(len) __noescape p);

// expected-expansion@+13:31{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public func swiftAttr(_ p: inout MutableSpan<Int32>) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = unsafe p.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe swiftAttr(len, _pPtr.baseAddress!)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void swiftAttr(int len, int *p) __attribute__((
    swift_attr("@_SwiftifyImport(.countedBy(pointer: .param(2), count: \"len\"), .nonescaping(pointer: .param(2)), spanAvailability: \"visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4\")")));

// expected-expansion@+22:98{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p1: copy p1) @_lifetime(p2: copy p2) @_disfavoredOverload public func shared(_ p1: inout MutableSpan<Int32>, _ p2: inout MutableSpan<Int32>) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p1.count)!|}}
//   expected-remark@4{{macro content: |    if p2.count != len {|}}
//   expected-remark@5{{macro content: |      fatalError("bounds check failure in shared: expected \\(len) but got \\(p2.count)")|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    let _p1Ptr = unsafe p1.withUnsafeMutableBufferPointer {|}}
//   expected-remark@8{{macro content: |        unsafe $0|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    defer {|}}
//   expected-remark@11{{macro content: |        _fixLifetime(p1)|}}
//   expected-remark@12{{macro content: |    }|}}
//   expected-remark@13{{macro content: |    let _p2Ptr = unsafe p2.withUnsafeMutableBufferPointer {|}}
//   expected-remark@14{{macro content: |        unsafe $0|}}
//   expected-remark@15{{macro content: |    }|}}
//   expected-remark@16{{macro content: |    defer {|}}
//   expected-remark@17{{macro content: |        _fixLifetime(p2)|}}
//   expected-remark@18{{macro content: |    }|}}
//   expected-remark@19{{macro content: |    return unsafe shared(len, _p1Ptr.baseAddress!, _p2Ptr.baseAddress!)|}}
//   expected-remark@20{{macro content: |}|}}
// }}
void shared(int len, int * __counted_by(len) __noescape p1, int * __counted_by(len) __noescape p2);

// expected-expansion@+16:84{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public func complexExpr(_ len: Int32, _ offset: Int32, _ p: inout MutableSpan<Int32>) {|}}
//   expected-remark@3{{macro content: |    let _pCount = p.count|}}
//   expected-remark@4{{macro content: |    if _pCount != len - offset {|}}
//   expected-remark@5{{macro content: |      fatalError("bounds check failure in complexExpr: expected \\(len - offset) but got \\(_pCount)")|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    let _pPtr = unsafe p.withUnsafeMutableBufferPointer {|}}
//   expected-remark@8{{macro content: |        unsafe $0|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    defer {|}}
//   expected-remark@11{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@12{{macro content: |    }|}}
//   expected-remark@13{{macro content: |    return unsafe complexExpr(len, offset, _pPtr.baseAddress!)|}}
//   expected-remark@14{{macro content: |}|}}
// }}
void complexExpr(int len, int offset, int * __counted_by(len - offset) __noescape p);

// expected-expansion@+13:85{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public func nullUnspecified(_ p: inout MutableSpan<Int32>) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = unsafe p.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe nullUnspecified(len, _pPtr.baseAddress!)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void nullUnspecified(int len, int * __counted_by(len) _Null_unspecified __noescape p);

// expected-expansion@+13:68{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public func nonnull(_ p: inout MutableSpan<Int32>) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = unsafe p.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe nonnull(len, _pPtr.baseAddress!)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void nonnull(int len, int * __counted_by(len) _Nonnull __noescape p);

// expected-expansion@+13:59{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public func nullable(_ p: inout MutableSpan<Int32>?) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p?.count ?? 0)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = unsafe p?.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe nullable(len, _pPtr?.baseAddress)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void nullable(int len, int * __counted_by(len) _Nullable p __noescape);

// expected-expansion@+6:57{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func returnPointer(_ len: Int32) -> UnsafeMutableBufferPointer<Int32> {|}}
//   expected-remark@3{{macro content: |    return unsafe UnsafeMutableBufferPointer<Int32>(start: unsafe returnPointer(len), count: Int(len))|}}
//   expected-remark@4{{macro content: |}|}}
// }}
int * __counted_by(len) __noescape returnPointer(int len);

// expected-expansion@+13:58{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(_anonymous_param1: copy _anonymous_param1) @_disfavoredOverload public func anonymous(_ _anonymous_param1: inout MutableSpan<Int32>?) {|}}
//   expected-remark@3{{macro content: |    let _anonymous_param0 = Int32(exactly: _anonymous_param1?.count ?? 0)!|}}
//   expected-remark@4{{macro content: |    let __anonymous_param1Ptr = unsafe _anonymous_param1?.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(_anonymous_param1)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe anonymous(_anonymous_param0, __anonymous_param1Ptr?.baseAddress)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void anonymous(int len, int * __counted_by(len) _Nullable __noescape);

void keyword(int len, int * __counted_by(len) _Nullable func __noescape,
    int extension,
    int init,
    int open,
    int var,
    int is,
    int as,
    int in,
    int guard,
    // expected-expansion@+13:14{{
    //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
    //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(`func`: copy `func`) @_disfavoredOverload public func keyword(_ `func`: inout MutableSpan<Int32>?, _ `extension`: Int32, _ `init`: Int32, _ open: Int32, _ `var`: Int32, _ `is`: Int32, _ `as`: Int32, _ `in`: Int32, _ `guard`: Int32, _ `where`: Int32) {|}}
    //   expected-remark@3{{macro content: |    let len = Int32(exactly: `func`?.count ?? 0)!|}}
    //   expected-remark@4{{macro content: |    let _funcPtr = unsafe `func`?.withUnsafeMutableBufferPointer {|}}
    //   expected-remark@5{{macro content: |        unsafe $0|}}
    //   expected-remark@6{{macro content: |    }|}}
    //   expected-remark@7{{macro content: |    defer {|}}
    //   expected-remark@8{{macro content: |        _fixLifetime(`func`)|}}
    //   expected-remark@9{{macro content: |    }|}}
    //   expected-remark@10{{macro content: |    return unsafe keyword(len, _funcPtr?.baseAddress, `extension`, `init`, open, `var`, `is`, `as`, `in`, `guard`, `where`)|}}
    //   expected-remark@11{{macro content: |}|}}
    // }}
    int where
);

// expected-expansion@+13:72{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(_pointerName_param1: copy _pointerName_param1) @_disfavoredOverload public func pointerName(_ _pointerName_param1: inout MutableSpan<Int32>?) {|}}
//   expected-remark@3{{macro content: |    let _pointerName_param0 = Int32(exactly: _pointerName_param1?.count ?? 0)!|}}
//   expected-remark@4{{macro content: |    let __pointerName_param1Ptr = unsafe _pointerName_param1?.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(_pointerName_param1)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe pointerName(_pointerName_param0, __pointerName_param1Ptr?.baseAddress)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void pointerName(int len, int * __counted_by(len) _Nullable pointerName __noescape);

// expected-expansion@+16:83{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(_lenName_param2: copy _lenName_param2) @_disfavoredOverload public func lenName(_ _lenName_param0: Int32, _ _lenName_param1: Int32, _ _lenName_param2: inout MutableSpan<Int32>?) {|}}
//   expected-remark@3{{macro content: |    let __lenName_param2Count = _lenName_param2?.count ?? 0|}}
//   expected-remark@4{{macro content: |    if __lenName_param2Count != _lenName_param0 * _lenName_param1 {|}}
//   expected-remark@5{{macro content: |      fatalError("bounds check failure in lenName: expected \\(_lenName_param0 * _lenName_param1) but got \\(__lenName_param2Count)")|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    let __lenName_param2Ptr = unsafe _lenName_param2?.withUnsafeMutableBufferPointer {|}}
//   expected-remark@8{{macro content: |        unsafe $0|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    defer {|}}
//   expected-remark@11{{macro content: |        _fixLifetime(_lenName_param2)|}}
//   expected-remark@12{{macro content: |    }|}}
//   expected-remark@13{{macro content: |    return unsafe lenName(_lenName_param0, _lenName_param1, __lenName_param2Ptr?.baseAddress)|}}
//   expected-remark@14{{macro content: |}|}}
// }}
void lenName(int lenName, int size, int * __counted_by(lenName * size) _Nullable p __noescape);

// expected-expansion@+13:58{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(_func_param1: copy _func_param1) @_disfavoredOverload public func `func`(_ _func_param1: inout MutableSpan<Int32>?) {|}}
//   expected-remark@3{{macro content: |    let _func_param0 = Int32(exactly: _func_param1?.count ?? 0)!|}}
//   expected-remark@4{{macro content: |    let __func_param1Ptr = unsafe _func_param1?.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(_func_param1)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe `func`(_func_param0, __func_param1Ptr?.baseAddress)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void func(int len, int * __counted_by(len) _Nullable func __noescape);

void *funcRenameKeyword(int len, int * __counted_by(len) _Nullable func __noescape,
    int extension,
    int init,
    int open,
    int var,
    int is,
    int as,
    int in,
    int guard,
    // expected-expansion@+13:14{{
    //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
    //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(`func`: copy `func`) @_disfavoredOverload public func funcRenamed(`func`: inout MutableSpan<Int32>?, `extension`: Int32, `init`: Int32, open: Int32, `var`: Int32, `is`: Int32, `as`: Int32, `in`: Int32, `guard`: Int32, `where`: Int32) -> UnsafeMutableRawPointer! {|}}
    //   expected-remark@3{{macro content: |    let len = Int32(exactly: `func`?.count ?? 0)!|}}
    //   expected-remark@4{{macro content: |    let _funcPtr = unsafe `func`?.withUnsafeMutableBufferPointer {|}}
    //   expected-remark@5{{macro content: |        unsafe $0|}}
    //   expected-remark@6{{macro content: |    }|}}
    //   expected-remark@7{{macro content: |    defer {|}}
    //   expected-remark@8{{macro content: |        _fixLifetime(`func`)|}}
    //   expected-remark@9{{macro content: |    }|}}
    //   expected-remark@10{{macro content: |    return unsafe funcRenamed(len: len, func: _funcPtr?.baseAddress, extension: `extension`, init: `init`, open: open, var: `var`, is: `is`, as: `as`, in: `in`, guard: `guard`, where: `where`)|}}
    //   expected-remark@11{{macro content: |}|}}
    // }}
    int where) __attribute__((swift_name("funcRenamed(len:func:extension:init:open:var:is:as:in:guard:where:)")));

void *funcRenameKeywordAnonymous(int len, int * __counted_by(len) _Nullable __noescape,
    int,
    int,
    int,
    int,
    int,
    int,
    int,
    int,
    // expected-expansion@+13:8{{
    //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
    //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(_funcRenamedAnon_param1: copy _funcRenamedAnon_param1) @_disfavoredOverload public func funcRenamedAnon(`func` _funcRenamedAnon_param1: inout MutableSpan<Int32>?, `extension` _funcRenamedAnon_param2: Int32, `init` _funcRenamedAnon_param3: Int32, open _funcRenamedAnon_param4: Int32, `var` _funcRenamedAnon_param5: Int32, `is` _funcRenamedAnon_param6: Int32, `as` _funcRenamedAnon_param7: Int32, `in` _funcRenamedAnon_param8: Int32, `guard` _funcRenamedAnon_param9: Int32, `where` _funcRenamedAnon_param10: Int32) -> UnsafeMutableRawPointer! {|}}
    //   expected-remark@3{{macro content: |    let _funcRenamedAnon_param0 = Int32(exactly: _funcRenamedAnon_param1?.count ?? 0)!|}}
    //   expected-remark@4{{macro content: |    let __funcRenamedAnon_param1Ptr = unsafe _funcRenamedAnon_param1?.withUnsafeMutableBufferPointer {|}}
    //   expected-remark@5{{macro content: |        unsafe $0|}}
    //   expected-remark@6{{macro content: |    }|}}
    //   expected-remark@7{{macro content: |    defer {|}}
    //   expected-remark@8{{macro content: |        _fixLifetime(_funcRenamedAnon_param1)|}}
    //   expected-remark@9{{macro content: |    }|}}
    //   expected-remark@10{{macro content: |    return unsafe funcRenamedAnon(len: _funcRenamedAnon_param0, func: __funcRenamedAnon_param1Ptr?.baseAddress, extension: _funcRenamedAnon_param2, init: _funcRenamedAnon_param3, open: _funcRenamedAnon_param4, var: _funcRenamedAnon_param5, is: _funcRenamedAnon_param6, as: _funcRenamedAnon_param7, in: _funcRenamedAnon_param8, guard: _funcRenamedAnon_param9, where: _funcRenamedAnon_param10)|}}
    //   expected-remark@11{{macro content: |}|}}
    // }}
    int) __attribute__((swift_name("funcRenamedAnon(len:func:extension:init:open:var:is:as:in:guard:where:)")));

// expected-expansion@+13:91{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(`func`: copy `func`) @_disfavoredOverload public func clash(`func`: inout MutableSpan<Int32>?, clash `where`: Int32) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: `func`?.count ?? 0)!|}}
//   expected-remark@4{{macro content: |    let _funcPtr = unsafe `func`?.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(`func`)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe clash(len: len, func: _funcPtr?.baseAddress, clash: `where`)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void funcRenameClash(int len, int * __counted_by(len) _Nullable func __noescape, int where)
    __attribute__((swift_name("clash(len:func:clash:)")));

// expected-expansion@+13:98{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(`func`: copy `func`) @_disfavoredOverload public func open(`func`: inout MutableSpan<Int32>?, open `where`: Int32) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: `func`?.count ?? 0)!|}}
//   expected-remark@4{{macro content: |    let _funcPtr = unsafe `func`?.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(`func`)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe open(len: len, func: _funcPtr?.baseAddress, open: `where`)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void funcRenameClashKeyword(int len, int * __counted_by(len) _Nullable func __noescape, int where)
    __attribute__((swift_name("open(len:func:open:)")));

// expected-expansion@+13:94{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(_clash2_param1: copy _clash2_param1) @_disfavoredOverload public func clash2(`func` _clash2_param1: inout MutableSpan<Int32>?, clash2 _clash2_param2: Int32) {|}}
//   expected-remark@3{{macro content: |    let _clash2_param0 = Int32(exactly: _clash2_param1?.count ?? 0)!|}}
//   expected-remark@4{{macro content: |    let __clash2_param1Ptr = unsafe _clash2_param1?.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(_clash2_param1)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe clash2(len: _clash2_param0, func: __clash2_param1Ptr?.baseAddress, clash2: _clash2_param2)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void funcRenameClashAnonymous(int len, int * __counted_by(len) _Nullable func __noescape, int)
    __attribute__((swift_name("clash2(len:func:clash2:)")));

// expected-expansion@+13:101{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(_in_param1: copy _in_param1) @_disfavoredOverload public func `in`(`func` _in_param1: inout MutableSpan<Int32>?, `in` _in_param2: Int32) {|}}
//   expected-remark@3{{macro content: |    let _in_param0 = Int32(exactly: _in_param1?.count ?? 0)!|}}
//   expected-remark@4{{macro content: |    let __in_param1Ptr = unsafe _in_param1?.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(_in_param1)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe `in`(len: _in_param0, func: __in_param1Ptr?.baseAddress, in: _in_param2)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void funcRenameClashKeywordAnonymous(int len, int * __counted_by(len) _Nullable func __noescape, int)
    __attribute__((swift_name("in(len:func:in:)")));

typedef struct actor_ *actor;
// expected-expansion@+15:94{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public func keywordType(_ p: inout MutableSpan<actor?>, _ p2: actor) -> actor {|}}
//   expected-warning@3{{expression uses unsafe constructs but is not marked with 'unsafe'}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = unsafe p.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-warning@8{{expression uses unsafe constructs but is not marked with 'unsafe'}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe keywordType(len, _pPtr.baseAddress!, p2)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
actor _Nonnull keywordType(int len, actor * __counted_by(len) __noescape p, actor _Nonnull p2);

//--- module.modulemap
module Test {
  header "test.h"
}

//--- test.swift
// GENERATED-BY: %target-swift-ide-test -print-module -module-to-print=Test -plugin-path %swift-plugin-dir -I %t -source-filename=x -enable-experimental-feature Lifetimes -Xcc -Wno-ignored-attributes -Xcc -Wno-nullability-completeness > %t/Test-interface.swift && %swift-function-caller-generator Test %t/Test-interface.swift
// GENERATED-HASH: 90e545e4d3b2456e1176ceb6f5f7b2b27f2aa3fe573c817c8d1f43939e6f1c6a
import Test

func call_simple(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe simple(len, p)
}

func call_swiftAttr(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe swiftAttr(len, p)
}

func call_shared(_ len: Int32, _ p1: UnsafeMutablePointer<Int32>!, _ p2: UnsafeMutablePointer<Int32>!) {
  return unsafe shared(len, p1, p2)
}

func call_complexExpr(_ len: Int32, _ offset: Int32, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe complexExpr(len, offset, p)
}

func call_nullUnspecified(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe nullUnspecified(len, p)
}

func call_nonnull(_ len: Int32, _ p: UnsafeMutablePointer<Int32>) {
  return unsafe nonnull(len, p)
}

func call_nullable(_ len: Int32, _ p: UnsafeMutablePointer<Int32>?) {
  return unsafe nullable(len, p)
}

func call_returnPointer(_ len: Int32) -> UnsafeMutablePointer<Int32>! {
  return unsafe returnPointer(len)
}

func call_anonymous(_ len: Int32, _ _anonymous_param1: UnsafeMutablePointer<Int32>?) {
  return unsafe anonymous(len,  _anonymous_param1)
}

func call_keyword(_ len: Int32, _ func: UnsafeMutablePointer<Int32>?, _ extension: Int32, _ init: Int32, _ open: Int32, _ var: Int32, _ is: Int32, _ as: Int32, _ in: Int32, _ guard: Int32, _ where: Int32) {
  return unsafe keyword(len, `func`, `extension`, `init`, open, `var`, `is`, `as`, `in`, `guard`, `where`)
}

func call_pointerName(_ len: Int32, _  _pointerName_param1: UnsafeMutablePointer<Int32>?) {
  return unsafe pointerName(len,  _pointerName_param1)
}

func call_lenName(_  _lenName_param0: Int32, _ size: Int32, _ p: UnsafeMutablePointer<Int32>?) {
  return unsafe lenName( _lenName_param0, size, p)
}

func call_func(_ len: Int32, _  _func_param1: UnsafeMutablePointer<Int32>?) {
  return unsafe `func`(len,  _func_param1)
}

func call_funcRenamed(len: Int32, func: UnsafeMutablePointer<Int32>?, extension: Int32, init: Int32, open: Int32, `var`: Int32, is: Int32, as: Int32, in: Int32, guard: Int32, where: Int32) -> UnsafeMutableRawPointer! {
  return unsafe funcRenamed(len: len, func: `func`, extension: `extension`, init: `init`, open: open, var: `var`, is: `is`, as: `as`, in: `in`, guard: `guard`, where: `where`)
}

func call_funcRenamedAnon(len: Int32, func  _funcRenamedAnon_param1: UnsafeMutablePointer<Int32>?, extension  _funcRenamedAnon_param2: Int32, init  _funcRenamedAnon_param3: Int32, open  _funcRenamedAnon_param4: Int32, `var`  _funcRenamedAnon_param5: Int32, is  _funcRenamedAnon_param6: Int32, as  _funcRenamedAnon_param7: Int32, in  _funcRenamedAnon_param8: Int32, guard  _funcRenamedAnon_param9: Int32, where  _funcRenamedAnon_param10: Int32) -> UnsafeMutableRawPointer! {
  return unsafe funcRenamedAnon(len: len, func:  _funcRenamedAnon_param1, extension:  _funcRenamedAnon_param2, init:  _funcRenamedAnon_param3, open:  _funcRenamedAnon_param4, var:  _funcRenamedAnon_param5, is:  _funcRenamedAnon_param6, as:  _funcRenamedAnon_param7, in:  _funcRenamedAnon_param8, guard:  _funcRenamedAnon_param9, where:  _funcRenamedAnon_param10)
}

func call_clash(len: Int32, func: UnsafeMutablePointer<Int32>?, clash where: Int32) {
  return unsafe clash(len: len, func: `func`, clash: `where`)
}

func call_open(len: Int32, func: UnsafeMutablePointer<Int32>?, open where: Int32) {
  return unsafe open(len: len, func: `func`, open: `where`)
}

func call_clash2(len: Int32, func: UnsafeMutablePointer<Int32>?, clash2  _clash2_param2: Int32) {
  return unsafe clash2(len: len, func: `func`, clash2:  _clash2_param2)
}

func call_in(len: Int32, func: UnsafeMutablePointer<Int32>?, in  _in_param2: Int32) {
  return unsafe `in`(len: len, func: `func`, in:  _in_param2)
}

func call_keywordType(_ len: Int32, _ p: UnsafeMutablePointer<actor?>!, _ p2: OpaquePointer) -> actor {
  return unsafe keywordType(len, p, p2)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(_anonymous_param1: copy _anonymous_param1)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_anonymous(_ _anonymous_param1: inout MutableSpan<Int32>?) {
  return anonymous(&_anonymous_param1)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(`func`: copy `func`)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_clash(func: inout MutableSpan<Int32>?, clash where: Int32) {
  return clash(func: &`func`, clash: `where`)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(_clash2_param1: copy _clash2_param1)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_clash2(func _clash2_param1: inout MutableSpan<Int32>?, clash2 _clash2_param2: Int32) {
  return clash2(func: &_clash2_param1, clash2: _clash2_param2)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_complexExpr(_ len: Int32, _ offset: Int32, _ p: inout MutableSpan<Int32>) {
  return complexExpr(len, offset, &p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(_func_param1: copy _func_param1)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_func(_ _func_param1: inout MutableSpan<Int32>?) {
  return `func`(&_func_param1)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(`func`: copy `func`)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_funcRenamed(func: inout MutableSpan<Int32>?, extension: Int32, init: Int32, open: Int32, `var`: Int32, is: Int32, as: Int32, in: Int32, guard: Int32, where: Int32) -> UnsafeMutableRawPointer! {
  return unsafe funcRenamed(func: &`func`, extension: `extension`, init: `init`, open: open, var: `var`, is: `is`, as: `as`, in: `in`, guard: `guard`, where: `where`)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(_funcRenamedAnon_param1: copy _funcRenamedAnon_param1)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_funcRenamedAnon(func _funcRenamedAnon_param1: inout MutableSpan<Int32>?, extension _funcRenamedAnon_param2: Int32, init _funcRenamedAnon_param3: Int32, open _funcRenamedAnon_param4: Int32, `var` _funcRenamedAnon_param5: Int32, is _funcRenamedAnon_param6: Int32, as _funcRenamedAnon_param7: Int32, in _funcRenamedAnon_param8: Int32, guard _funcRenamedAnon_param9: Int32, where _funcRenamedAnon_param10: Int32) -> UnsafeMutableRawPointer! {
  return unsafe funcRenamedAnon(func: &_funcRenamedAnon_param1, extension: _funcRenamedAnon_param2, init: _funcRenamedAnon_param3, open: _funcRenamedAnon_param4, var: _funcRenamedAnon_param5, is: _funcRenamedAnon_param6, as: _funcRenamedAnon_param7, in: _funcRenamedAnon_param8, guard: _funcRenamedAnon_param9, where: _funcRenamedAnon_param10)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(_in_param1: copy _in_param1)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_in(func _in_param1: inout MutableSpan<Int32>?, in _in_param2: Int32) {
  return `in`(func: &_in_param1, in: _in_param2)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(`func`: copy `func`)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_keyword(_ func: inout MutableSpan<Int32>?, _ extension: Int32, _ init: Int32, _ open: Int32, _ var: Int32, _ is: Int32, _ as: Int32, _ in: Int32, _ guard: Int32, _ where: Int32) {
  return keyword(&`func`, `extension`, `init`, open, `var`, `is`, `as`, `in`, `guard`, `where`)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_keywordType(_ p: inout MutableSpan<actor?>, _ p2: OpaquePointer) -> actor {
  return unsafe keywordType(&p, p2)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(_lenName_param2: copy _lenName_param2)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_lenName(_ _lenName_param0: Int32, _ _lenName_param1: Int32, _ _lenName_param2: inout MutableSpan<Int32>?) {
  return lenName(_lenName_param0, _lenName_param1, &_lenName_param2)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nonnull(_ p: inout MutableSpan<Int32>) {
  return nonnull(&p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nullUnspecified(_ p: inout MutableSpan<Int32>) {
  return nullUnspecified(&p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nullable(_ p: inout MutableSpan<Int32>?) {
  return nullable(&p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(`func`: copy `func`)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_open(func: inout MutableSpan<Int32>?, open where: Int32) {
  return open(func: &`func`, open: `where`)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(_pointerName_param1: copy _pointerName_param1)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_pointerName(_ _pointerName_param1: inout MutableSpan<Int32>?) {
  return pointerName(&_pointerName_param1)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_returnPointer(_ len: Int32) -> UnsafeMutableBufferPointer<Int32> {
  return unsafe returnPointer(len)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(p1: copy p1)
@_lifetime(p2: copy p2)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_shared(_ p1: inout MutableSpan<Int32>, _ p2: inout MutableSpan<Int32>) {
  return shared(&p1, &p2)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_simple(_ p: inout MutableSpan<Int32>) {
  return simple(&p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_swiftAttr(_ p: inout MutableSpan<Int32>) {
  return swiftAttr(&p)
}
