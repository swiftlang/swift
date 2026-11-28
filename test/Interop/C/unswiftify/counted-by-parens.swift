// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_SafeInteropImplementations

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-verify-unswiftify %t/test.swift -import-bridging-header %t/test.h -verify-additional-file %t/test.h -enable-experimental-feature SafeInteropImplementations

//--- test.h
// The extra (redundant) parentheses results in _SwiftifyImport not eliding the
// count parameter from the safe wrapper. _Unswiftify needs to match this behavior.
#define CB __attribute__((__counted_by__((n))))

// expected-expansion@+15:49{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func redundant_parens(_ p: UnsafeMutableBufferPointer<CInt>, _ n: CInt) {|}}
//   expected-remark@3{{macro content: |    if p.count != (n) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("redundant_parens", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((n), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    return unsafe redundant_parens(p.baseAddress!, n)|}}
//   expected-remark@13{{macro content: |}|}}
// }}
void redundant_parens(int * _Nonnull CB p, int n);

//--- test.swift
@c @implementation
// expected-expansion@+9:82{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func redundant_parens(_ p: UnsafeMutablePointer<CInt>, _ n: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = Int((n))|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-remark@5{{macro content: |    let _safeArg0 = unsafe UnsafeMutableBufferPointer(start: p, count: _count0)|}}
//   expected-remark@6{{macro content: |    unsafe redundant_parens(_safeArg0, n)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
public func redundant_parens(_ p: UnsafeMutableBufferPointer<CInt>, _ n: CInt) {}
