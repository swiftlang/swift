// REQUIRES: std_span

// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -c -plugin-path %swift-plugin-dir -I %t%{fs-sep}Inputs -Xcc -std=c++20 -cxx-interoperability-mode=default %t/method.swift \
// RUN:   -Rmacro-expansions -verify -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}method.h -suppress-notes


//--- Inputs/module.modulemap
module Method {
    header "method.h"
    requires cplusplus
}

//--- Inputs/method.h
#include <span>

using IntSpan = std::span<const int>;

class Foo {
public:
   Foo();
// expected-expansion@+7:18{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload|}}
//   expected-remark@3{{macro content: |public init(_ sp: Span<CInt>) {|}}
//   expected-remark@4{{macro content: |    unsafe self.init(IntSpan(sp))|}}
//   expected-remark@5{{macro content: |}|}}
// }}
   Foo(IntSpan sp [[clang::noescape]]);
};

//--- method.swift
import CxxStdlib
import Method
  
func test(s: Span<Int32>) {
  var _ = Foo(s)
}
