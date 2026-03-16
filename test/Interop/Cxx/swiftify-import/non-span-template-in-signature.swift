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
  std::optional<size_t> process(ByteSpan input __noescape,
                                MutableByteSpan output __noescape) const;
};

// Class-member type aliases for std::span must also work.
struct Bar {
  using ConstSpan = std::span<const uint8_t>;
  using MutSpan = std::span<uint8_t>;

  void readData(ConstSpan data __noescape) const;
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
