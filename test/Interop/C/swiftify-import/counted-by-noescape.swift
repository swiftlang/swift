// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_SafeInteropWrappersNullAsEmptySpan

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -enable-experimental-feature Lifetimes -enable-experimental-feature SafeInteropWrappersNullAsEmptySpan -strict-memory-safety -Xcc -Wno-ignored-attributes -Xcc -Wno-nullability-completeness \
// RUN:   %t/test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -suppress-notes -eager-macro-checking

// Check that ClangImporter correctly infers and expands @_SwiftifyImport macros for functions with __counted_by __noescape parameters.

//--- test.h
#pragma once

#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __noescape __attribute__((noescape))

// expected-expansion@+13:58{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public func simple(_ p: inout MutableSpan<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe simple(len, _pPtr.baseAddress)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void simple(int len, int * __counted_by(len) __noescape p);

// expected-expansion@+13:31{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public func swiftAttr(_ p: inout MutableSpan<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe swiftAttr(len, _pPtr.baseAddress)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void swiftAttr(int len, int *p) __attribute__((
    swift_attr("@_SwiftifyImport(.countedBy(pointer: .param(2), count: \"len\"), .nonescaping(pointer: .param(2)), spanAvailability: \"visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4\")")));

// expected-expansion@+22:98{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p1: copy p1) @_lifetime(p2: copy p2) @_disfavoredOverload public func shared(_ p1: inout MutableSpan<CInt>, _ p2: inout MutableSpan<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p2.count)!|}}
//   expected-remark@4{{macro content: |    if p1.count != len {|}}
//   expected-remark@5{{macro content: |      fatalError("bounds check failure in shared: expected \\(len) but got \\(p1.count)")|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    let _p1Ptr = p1.withUnsafeMutableBufferPointer {|}}
//   expected-remark@8{{macro content: |        unsafe $0|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    defer {|}}
//   expected-remark@11{{macro content: |        _fixLifetime(p1)|}}
//   expected-remark@12{{macro content: |    }|}}
//   expected-remark@13{{macro content: |    let _p2Ptr = p2.withUnsafeMutableBufferPointer {|}}
//   expected-remark@14{{macro content: |        unsafe $0|}}
//   expected-remark@15{{macro content: |    }|}}
//   expected-remark@16{{macro content: |    defer {|}}
//   expected-remark@17{{macro content: |        _fixLifetime(p2)|}}
//   expected-remark@18{{macro content: |    }|}}
//   expected-remark@19{{macro content: |    return unsafe shared(len, _p1Ptr.baseAddress, _p2Ptr.baseAddress)|}}
//   expected-remark@20{{macro content: |}|}}
// }}
void shared(int len, int * __counted_by(len) __noescape p1, int * __counted_by(len) __noescape p2);

// expected-expansion@+15:84{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public func complexExpr(_ len: CInt, _ offset: CInt, _ p: inout MutableSpan<CInt>) {|}}
//   expected-remark@3{{macro content: |    if p.count != (len - offset) {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in complexExpr: expected \\((len - offset)) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-remark@7{{macro content: |        unsafe $0|}}
//   expected-remark@8{{macro content: |    }|}}
//   expected-remark@9{{macro content: |    defer {|}}
//   expected-remark@10{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    return unsafe complexExpr(len, offset, _pPtr.baseAddress)|}}
//   expected-remark@13{{macro content: |}|}}
// }}
void complexExpr(int len, int offset, int * __counted_by(len - offset) __noescape p);

// expected-expansion@+13:85{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public func nullUnspecified(_ p: inout MutableSpan<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe nullUnspecified(len, _pPtr.baseAddress)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void nullUnspecified(int len, int * __counted_by(len) _Null_unspecified __noescape p);

// expected-expansion@+13:68{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public func nonnull(_ p: inout MutableSpan<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
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
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public func nullable(_ p: inout MutableSpan<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe nullable(len, _pPtr.baseAddress)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void nullable(int len, int * __counted_by(len) _Nullable p __noescape);

// expected-expansion@+6:57{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func returnPointer(_ len: CInt) -> UnsafeMutableBufferPointer<CInt> {|}}
//   expected-remark@3{{macro content: |    return unsafe UnsafeMutableBufferPointer<CInt>(start: unsafe returnPointer(len), count: Int(len))|}}
//   expected-remark@4{{macro content: |}|}}
// }}
int * __counted_by(len) __noescape returnPointer(int len);

// expected-expansion@+13:58{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(_anonymous_param1: copy _anonymous_param1) @_disfavoredOverload public func anonymous(_ _anonymous_param1: inout MutableSpan<CInt>) {|}}
//   expected-remark@3{{macro content: |    let _anonymous_param0 = CInt(exactly: _anonymous_param1.count)!|}}
//   expected-remark@4{{macro content: |    let __anonymous_param1Ptr = _anonymous_param1.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(_anonymous_param1)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe anonymous(_anonymous_param0, __anonymous_param1Ptr.baseAddress)|}}
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
    //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(`func`: copy `func`) @_disfavoredOverload public func keyword(_ `func`: inout MutableSpan<CInt>, _ `extension`: CInt, _ `init`: CInt, _ open: CInt, _ `var`: CInt, _ `is`: CInt, _ `as`: CInt, _ `in`: CInt, _ `guard`: CInt, _ `where`: CInt) {|}}
    //   expected-remark@3{{macro content: |    let len = CInt(exactly: `func`.count)!|}}
    //   expected-remark@4{{macro content: |    let _funcPtr = `func`.withUnsafeMutableBufferPointer {|}}
    //   expected-remark@5{{macro content: |        unsafe $0|}}
    //   expected-remark@6{{macro content: |    }|}}
    //   expected-remark@7{{macro content: |    defer {|}}
    //   expected-remark@8{{macro content: |        _fixLifetime(`func`)|}}
    //   expected-remark@9{{macro content: |    }|}}
    //   expected-remark@10{{macro content: |    return unsafe keyword(len, _funcPtr.baseAddress, `extension`, `init`, open, `var`, `is`, `as`, `in`, `guard`, `where`)|}}
    //   expected-remark@11{{macro content: |}|}}
    // }}
    int where
);

// expected-expansion@+13:72{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(_pointerName_param1: copy _pointerName_param1) @_disfavoredOverload public func pointerName(_ _pointerName_param1: inout MutableSpan<CInt>) {|}}
//   expected-remark@3{{macro content: |    let _pointerName_param0 = CInt(exactly: _pointerName_param1.count)!|}}
//   expected-remark@4{{macro content: |    let __pointerName_param1Ptr = _pointerName_param1.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(_pointerName_param1)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe pointerName(_pointerName_param0, __pointerName_param1Ptr.baseAddress)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void pointerName(int len, int * __counted_by(len) _Nullable pointerName __noescape);

// expected-expansion@+15:83{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(_lenName_param2: copy _lenName_param2) @_disfavoredOverload public func lenName(_ _lenName_param0: CInt, _ _lenName_param1: CInt, _ _lenName_param2: inout MutableSpan<CInt>) {|}}
//   expected-remark@3{{macro content: |    if _lenName_param2.count != (_lenName_param0 * _lenName_param1) {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in lenName: expected \\((_lenName_param0 * _lenName_param1)) but got \\(_lenName_param2.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    let __lenName_param2Ptr = _lenName_param2.withUnsafeMutableBufferPointer {|}}
//   expected-remark@7{{macro content: |        unsafe $0|}}
//   expected-remark@8{{macro content: |    }|}}
//   expected-remark@9{{macro content: |    defer {|}}
//   expected-remark@10{{macro content: |        _fixLifetime(_lenName_param2)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    return unsafe lenName(_lenName_param0, _lenName_param1, __lenName_param2Ptr.baseAddress)|}}
//   expected-remark@13{{macro content: |}|}}
// }}
void lenName(int lenName, int size, int * __counted_by(lenName * size) _Nullable p __noescape);

// expected-expansion@+13:58{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(_func_param1: copy _func_param1) @_disfavoredOverload public func `func`(_ _func_param1: inout MutableSpan<CInt>) {|}}
//   expected-remark@3{{macro content: |    let _func_param0 = CInt(exactly: _func_param1.count)!|}}
//   expected-remark@4{{macro content: |    let __func_param1Ptr = _func_param1.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(_func_param1)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe `func`(_func_param0, __func_param1Ptr.baseAddress)|}}
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
    //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(`func`: copy `func`) @_disfavoredOverload public func funcRenamed(`func`: inout MutableSpan<CInt>, `extension`: CInt, `init`: CInt, open: CInt, `var`: CInt, `is`: CInt, `as`: CInt, `in`: CInt, `guard`: CInt, `where`: CInt) -> UnsafeMutableRawPointer! {|}}
    //   expected-remark@3{{macro content: |    let len = CInt(exactly: `func`.count)!|}}
    //   expected-remark@4{{macro content: |    let _funcPtr = `func`.withUnsafeMutableBufferPointer {|}}
    //   expected-remark@5{{macro content: |        unsafe $0|}}
    //   expected-remark@6{{macro content: |    }|}}
    //   expected-remark@7{{macro content: |    defer {|}}
    //   expected-remark@8{{macro content: |        _fixLifetime(`func`)|}}
    //   expected-remark@9{{macro content: |    }|}}
    //   expected-remark@10{{macro content: |    return unsafe funcRenamed(len: len, func: _funcPtr.baseAddress, extension: `extension`, init: `init`, open: open, var: `var`, is: `is`, as: `as`, in: `in`, guard: `guard`, where: `where`)|}}
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
    //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(_funcRenamedAnon_param1: copy _funcRenamedAnon_param1) @_disfavoredOverload public func funcRenamedAnon(`func` _funcRenamedAnon_param1: inout MutableSpan<CInt>, `extension` _funcRenamedAnon_param2: CInt, `init` _funcRenamedAnon_param3: CInt, open _funcRenamedAnon_param4: CInt, `var` _funcRenamedAnon_param5: CInt, `is` _funcRenamedAnon_param6: CInt, `as` _funcRenamedAnon_param7: CInt, `in` _funcRenamedAnon_param8: CInt, `guard` _funcRenamedAnon_param9: CInt, `where` _funcRenamedAnon_param10: CInt) -> UnsafeMutableRawPointer! {|}}
    //   expected-remark@3{{macro content: |    let _funcRenamedAnon_param0 = CInt(exactly: _funcRenamedAnon_param1.count)!|}}
    //   expected-remark@4{{macro content: |    let __funcRenamedAnon_param1Ptr = _funcRenamedAnon_param1.withUnsafeMutableBufferPointer {|}}
    //   expected-remark@5{{macro content: |        unsafe $0|}}
    //   expected-remark@6{{macro content: |    }|}}
    //   expected-remark@7{{macro content: |    defer {|}}
    //   expected-remark@8{{macro content: |        _fixLifetime(_funcRenamedAnon_param1)|}}
    //   expected-remark@9{{macro content: |    }|}}
    //   expected-remark@10{{macro content: |    return unsafe funcRenamedAnon(len: _funcRenamedAnon_param0, func: __funcRenamedAnon_param1Ptr.baseAddress, extension: _funcRenamedAnon_param2, init: _funcRenamedAnon_param3, open: _funcRenamedAnon_param4, var: _funcRenamedAnon_param5, is: _funcRenamedAnon_param6, as: _funcRenamedAnon_param7, in: _funcRenamedAnon_param8, guard: _funcRenamedAnon_param9, where: _funcRenamedAnon_param10)|}}
    //   expected-remark@11{{macro content: |}|}}
    // }}
    int) __attribute__((swift_name("funcRenamedAnon(len:func:extension:init:open:var:is:as:in:guard:where:)")));

// expected-expansion@+13:91{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(`func`: copy `func`) @_disfavoredOverload public func clash(`func`: inout MutableSpan<CInt>, clash `where`: CInt) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: `func`.count)!|}}
//   expected-remark@4{{macro content: |    let _funcPtr = `func`.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(`func`)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe clash(len: len, func: _funcPtr.baseAddress, clash: `where`)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void funcRenameClash(int len, int * __counted_by(len) _Nullable func __noescape, int where)
    __attribute__((swift_name("clash(len:func:clash:)")));

// expected-expansion@+13:98{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(`func`: copy `func`) @_disfavoredOverload public func open(`func`: inout MutableSpan<CInt>, open `where`: CInt) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: `func`.count)!|}}
//   expected-remark@4{{macro content: |    let _funcPtr = `func`.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(`func`)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe open(len: len, func: _funcPtr.baseAddress, open: `where`)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void funcRenameClashKeyword(int len, int * __counted_by(len) _Nullable func __noescape, int where)
    __attribute__((swift_name("open(len:func:open:)")));

// expected-expansion@+13:94{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(_clash2_param1: copy _clash2_param1) @_disfavoredOverload public func clash2(`func` _clash2_param1: inout MutableSpan<CInt>, clash2 _clash2_param2: CInt) {|}}
//   expected-remark@3{{macro content: |    let _clash2_param0 = CInt(exactly: _clash2_param1.count)!|}}
//   expected-remark@4{{macro content: |    let __clash2_param1Ptr = _clash2_param1.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(_clash2_param1)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe clash2(len: _clash2_param0, func: __clash2_param1Ptr.baseAddress, clash2: _clash2_param2)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void funcRenameClashAnonymous(int len, int * __counted_by(len) _Nullable func __noescape, int)
    __attribute__((swift_name("clash2(len:func:clash2:)")));

// expected-expansion@+13:101{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(_in_param1: copy _in_param1) @_disfavoredOverload public func `in`(`func` _in_param1: inout MutableSpan<CInt>, `in` _in_param2: CInt) {|}}
//   expected-remark@3{{macro content: |    let _in_param0 = CInt(exactly: _in_param1.count)!|}}
//   expected-remark@4{{macro content: |    let __in_param1Ptr = _in_param1.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(_in_param1)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe `in`(len: _in_param0, func: __in_param1Ptr.baseAddress, in: _in_param2)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void funcRenameClashKeywordAnonymous(int len, int * __counted_by(len) _Nullable func __noescape, int)
    __attribute__((swift_name("in(len:func:in:)")));

typedef struct actor_ *actor;
// expected-expansion@+15:94{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public func keywordType(_ p: inout MutableSpan<actor?>, _ p2: actor) -> actor {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-warning@3{{expression uses unsafe constructs but is not marked with 'unsafe'}}
//   expected-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-warning@8{{expression uses unsafe constructs but is not marked with 'unsafe'}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe keywordType(len, _pPtr.baseAddress, p2)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
actor _Nonnull keywordType(int len, actor * __counted_by(len) __noescape p, actor _Nonnull p2);

//--- module.modulemap
module Test {
  header "test.h"
}

//--- test.swift
// GENERATED-BY: %target-swift-ide-test -print-module -module-to-print=Test -plugin-path %swift-plugin-dir -I %t -source-filename=x -enable-experimental-feature Lifetimes -Xcc -Wno-ignored-attributes -Xcc -Wno-nullability-completeness > %t/Test-interface.swift && %swift-function-caller-generator Test %t/Test-interface.swift
// GENERATED-HASH: e996ede98d4d1961fe5a46e78c9b00418d1dc8dcc5b2d4edb8fa303f574b36df
import Test

func call_simple(_ len: CInt, _ p: UnsafeMutablePointer<CInt>!) {
  return unsafe simple(len, p)
}

func call_swiftAttr(_ len: CInt, _ p: UnsafeMutablePointer<CInt>!) {
  return unsafe swiftAttr(len, p)
}

func call_shared(_ len: CInt, _ p1: UnsafeMutablePointer<CInt>!, _ p2: UnsafeMutablePointer<CInt>!) {
  return unsafe shared(len, p1, p2)
}

func call_complexExpr(_ len: CInt, _ offset: CInt, _ p: UnsafeMutablePointer<CInt>!) {
  return unsafe complexExpr(len, offset, p)
}

func call_nullUnspecified(_ len: CInt, _ p: UnsafeMutablePointer<CInt>!) {
  return unsafe nullUnspecified(len, p)
}

func call_nonnull(_ len: CInt, _ p: UnsafeMutablePointer<CInt>) {
  return unsafe nonnull(len, p)
}

func call_nullable(_ len: CInt, _ p: UnsafeMutablePointer<CInt>?) {
  return unsafe nullable(len, p)
}

func call_returnPointer(_ len: CInt) -> UnsafeMutablePointer<CInt>! {
  return unsafe returnPointer(len)
}

func call_anonymous(_ len: CInt, _ _anonymous_param1: UnsafeMutablePointer<CInt>?) {
  return unsafe anonymous(len,  _anonymous_param1)
}

func call_keyword(_ len: CInt, _ func: UnsafeMutablePointer<CInt>?, _ extension: CInt, _ init: CInt, _ open: CInt, _ var: CInt, _ is: CInt, _ as: CInt, _ in: CInt, _ guard: CInt, _ where: CInt) {
  return unsafe keyword(len, `func`, `extension`, `init`, open, `var`, `is`, `as`, `in`, `guard`, `where`)
}

func call_pointerName(_ len: CInt, _  _pointerName_param1: UnsafeMutablePointer<CInt>?) {
  return unsafe pointerName(len,  _pointerName_param1)
}

func call_lenName(_  _lenName_param0: CInt, _ size: CInt, _ p: UnsafeMutablePointer<CInt>?) {
  return unsafe lenName( _lenName_param0, size, p)
}

func call_func(_ len: CInt, _  _func_param1: UnsafeMutablePointer<CInt>?) {
  return unsafe `func`(len,  _func_param1)
}

func call_funcRenamed(len: CInt, func: UnsafeMutablePointer<CInt>?, extension: CInt, init: CInt, open: CInt, `var`: CInt, is: CInt, as: CInt, in: CInt, guard: CInt, where: CInt) -> UnsafeMutableRawPointer! {
  return unsafe funcRenamed(len: len, func: `func`, extension: `extension`, init: `init`, open: open, var: `var`, is: `is`, as: `as`, in: `in`, guard: `guard`, where: `where`)
}

func call_funcRenamedAnon(len: CInt, func  _funcRenamedAnon_param1: UnsafeMutablePointer<CInt>?, extension  _funcRenamedAnon_param2: CInt, init  _funcRenamedAnon_param3: CInt, open  _funcRenamedAnon_param4: CInt, `var`  _funcRenamedAnon_param5: CInt, is  _funcRenamedAnon_param6: CInt, as  _funcRenamedAnon_param7: CInt, in  _funcRenamedAnon_param8: CInt, guard  _funcRenamedAnon_param9: CInt, where  _funcRenamedAnon_param10: CInt) -> UnsafeMutableRawPointer! {
  return unsafe funcRenamedAnon(len: len, func:  _funcRenamedAnon_param1, extension:  _funcRenamedAnon_param2, init:  _funcRenamedAnon_param3, open:  _funcRenamedAnon_param4, var:  _funcRenamedAnon_param5, is:  _funcRenamedAnon_param6, as:  _funcRenamedAnon_param7, in:  _funcRenamedAnon_param8, guard:  _funcRenamedAnon_param9, where:  _funcRenamedAnon_param10)
}

func call_clash(len: CInt, func: UnsafeMutablePointer<CInt>?, clash where: CInt) {
  return unsafe clash(len: len, func: `func`, clash: `where`)
}

func call_open(len: CInt, func: UnsafeMutablePointer<CInt>?, open where: CInt) {
  return unsafe open(len: len, func: `func`, open: `where`)
}

func call_clash2(len: CInt, func: UnsafeMutablePointer<CInt>?, clash2  _clash2_param2: CInt) {
  return unsafe clash2(len: len, func: `func`, clash2:  _clash2_param2)
}

func call_in(len: CInt, func: UnsafeMutablePointer<CInt>?, in  _in_param2: CInt) {
  return unsafe `in`(len: len, func: `func`, in:  _in_param2)
}

func call_keywordType(_ len: CInt, _ p: UnsafeMutablePointer<actor?>!, _ p2: OpaquePointer) -> actor {
  return unsafe keywordType(len, p, p2)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(_anonymous_param1: copy _anonymous_param1)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_anonymous(_ _anonymous_param1: inout MutableSpan<CInt>) {
  return anonymous(&_anonymous_param1)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(`func`: copy `func`)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_clash(func: inout MutableSpan<CInt>, clash where: CInt) {
  return clash(func: &`func`, clash: `where`)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(_clash2_param1: copy _clash2_param1)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_clash2(func _clash2_param1: inout MutableSpan<CInt>, clash2 _clash2_param2: CInt) {
  return clash2(func: &_clash2_param1, clash2: _clash2_param2)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_complexExpr(_ len: CInt, _ offset: CInt, _ p: inout MutableSpan<CInt>) {
  return complexExpr(len, offset, &p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(_func_param1: copy _func_param1)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_func(_ _func_param1: inout MutableSpan<CInt>) {
  return `func`(&_func_param1)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(`func`: copy `func`)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_funcRenamed(func: inout MutableSpan<CInt>, extension: CInt, init: CInt, open: CInt, `var`: CInt, is: CInt, as: CInt, in: CInt, guard: CInt, where: CInt) -> UnsafeMutableRawPointer! {
  return unsafe funcRenamed(func: &`func`, extension: `extension`, init: `init`, open: open, var: `var`, is: `is`, as: `as`, in: `in`, guard: `guard`, where: `where`)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(_funcRenamedAnon_param1: copy _funcRenamedAnon_param1)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_funcRenamedAnon(func _funcRenamedAnon_param1: inout MutableSpan<CInt>, extension _funcRenamedAnon_param2: CInt, init _funcRenamedAnon_param3: CInt, open _funcRenamedAnon_param4: CInt, `var` _funcRenamedAnon_param5: CInt, is _funcRenamedAnon_param6: CInt, as _funcRenamedAnon_param7: CInt, in _funcRenamedAnon_param8: CInt, guard _funcRenamedAnon_param9: CInt, where _funcRenamedAnon_param10: CInt) -> UnsafeMutableRawPointer! {
  return unsafe funcRenamedAnon(func: &_funcRenamedAnon_param1, extension: _funcRenamedAnon_param2, init: _funcRenamedAnon_param3, open: _funcRenamedAnon_param4, var: _funcRenamedAnon_param5, is: _funcRenamedAnon_param6, as: _funcRenamedAnon_param7, in: _funcRenamedAnon_param8, guard: _funcRenamedAnon_param9, where: _funcRenamedAnon_param10)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(_in_param1: copy _in_param1)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_in(func _in_param1: inout MutableSpan<CInt>, in _in_param2: CInt) {
  return `in`(func: &_in_param1, in: _in_param2)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(`func`: copy `func`)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_keyword(_ func: inout MutableSpan<CInt>, _ extension: CInt, _ init: CInt, _ open: CInt, _ var: CInt, _ is: CInt, _ as: CInt, _ in: CInt, _ guard: CInt, _ where: CInt) {
  return keyword(&`func`, `extension`, `init`, open, `var`, `is`, `as`, `in`, `guard`, `where`)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_keywordType(_ p: inout MutableSpan<actor?>, _ p2: OpaquePointer) -> actor {
  return unsafe keywordType(&p, p2)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(_lenName_param2: copy _lenName_param2)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_lenName(_ _lenName_param0: CInt, _ _lenName_param1: CInt, _ _lenName_param2: inout MutableSpan<CInt>) {
  return lenName(_lenName_param0, _lenName_param1, &_lenName_param2)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nonnull(_ p: inout MutableSpan<CInt>) {
  return nonnull(&p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nullUnspecified(_ p: inout MutableSpan<CInt>) {
  return nullUnspecified(&p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nullable(_ p: inout MutableSpan<CInt>) {
  return nullable(&p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(`func`: copy `func`)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_open(func: inout MutableSpan<CInt>, open where: CInt) {
  return open(func: &`func`, open: `where`)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(_pointerName_param1: copy _pointerName_param1)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_pointerName(_ _pointerName_param1: inout MutableSpan<CInt>) {
  return pointerName(&_pointerName_param1)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_returnPointer(_ len: CInt) -> UnsafeMutableBufferPointer<CInt> {
  return unsafe returnPointer(len)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(p1: copy p1)
@_lifetime(p2: copy p2)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_shared(_ p1: inout MutableSpan<CInt>, _ p2: inout MutableSpan<CInt>) {
  return shared(&p1, &p2)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_simple(_ p: inout MutableSpan<CInt>) {
  return simple(&p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_swiftAttr(_ p: inout MutableSpan<CInt>) {
  return swiftAttr(&p)
}
