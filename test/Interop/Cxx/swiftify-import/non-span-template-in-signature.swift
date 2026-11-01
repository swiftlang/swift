// Regression test: SafeInteropWrappers must generate Span wrappers even when
// non-span templates like std::optional appear in the function signature, and
// when std::span type aliases are defined at class scope.

// REQUIRES: std_span
// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_Lifetimes

// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -c -plugin-path %swift-plugin-dir -I %t%{fs-sep}Inputs \
// RUN:   -Xcc -std=c++20 \
// RUN:   -enable-experimental-feature SafeInteropWrappers \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -Rmacro-expansions -verify -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}api.h \
// RUN:   -suppress-notes -eager-macro-checking \
// RUN:   %t/test.swift

//--- Inputs/module.modulemap
module TestApi {
    header "api.h"
    requires cplusplus
}

//--- Inputs/api.h
#include <cstdint>
#include <optional>
#include <span>
#include <lifetimebound.h>

using ByteSpan = std::span<const uint8_t>;
using MutableByteSpan = std::span<uint8_t>;

// Non-span templates in the return type must not block wrapper generation.
struct Foo {
  // expected-expansion@+9:82{{
  //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(output: copy output) @_disfavoredOverload|}}
  //   expected-remark@3{{macro content: |public func process(_ input: Span<CUnsignedChar>, _ output: inout MutableSpan<CUnsignedChar>) -> std.__1.optional<CUnsignedLong> {|}}
  //   expected-remark@4{{macro content: |    return unsafe output.withUnsafeMutableBufferPointer { _outputPtr in|}}
  //   expected-remark@5{{macro content: |      return unsafe process(ByteSpan(input), MutableByteSpan(_outputPtr))|}}
  //   expected-remark@6{{macro content: |    }|}}
  //   expected-remark@7{{macro content: |}|}}
  // }}
  std::optional<size_t> process(ByteSpan input __noescape, MutableByteSpan output __noescape) const;
};

// Class-member type aliases for std::span must also work.
struct Bar {
  using ConstSpan = std::span<const uint8_t>;
  using MutSpan = std::span<uint8_t>;

  // expected-expansion@+7:31{{
  //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload|}}
  //   expected-remark@3{{macro content: |public func readData(_ data: Span<CUnsignedChar>) {|}}
  //   expected-remark@4{{macro content: |    return unsafe readData(Bar.ConstSpan(data))|}}
  //   expected-remark@5{{macro content: |}|}}
  // }}
  void readData(ConstSpan data __noescape) const;

  // expected-expansion@+9:32{{
  //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(output: copy output) @_disfavoredOverload|}}
  //   expected-remark@3{{macro content: |public func writeData(_ output: inout MutableSpan<CUnsignedChar>) {|}}
  //   expected-remark@4{{macro content: |    return unsafe output.withUnsafeMutableBufferPointer { _outputPtr in|}}
  //   expected-remark@5{{macro content: |      return unsafe writeData(Bar.MutSpan(_outputPtr))|}}
  //   expected-remark@6{{macro content: |    }|}}
  //   expected-remark@7{{macro content: |}|}}
  // }}
  void writeData(MutSpan output __noescape) const;
};

//--- test.swift
import CxxStdlib
import TestApi

@_lifetime(output: copy output)
func testProcess(_ foo: borrowing Foo, _ input: Span<UInt8>,
                 _ output: inout MutableSpan<UInt8>) {
  _ = foo.process(input, &output)
}

func testRead(_ bar: borrowing Bar, _ data: Span<UInt8>) {
  bar.readData(data)
}

func testWrite(_ bar: borrowing Bar, _ output: inout MutableSpan<UInt8>) {
  bar.writeData(&output)
}
