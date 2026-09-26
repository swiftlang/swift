// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend %t/use-templates.swift -module-name UseTemplates -typecheck -verify -emit-clang-header-path %t/UseTemplates.h -I %t -cxx-interoperability-mode=default -clang-header-expose-decls=all-public
// RUN: %FileCheck %s --input-file %t/UseTemplates.h
// RUN: %check-interop-cxx-header-in-clang(%t/UseTemplates.h -include %t/templates.h)

//--- templates.h
namespace test {
template<class... Types> struct Pack { int value; };
template<class Head, class... Tail> struct HeadAndPack { int value; };
template<int... Values> struct ValuePack { int value; };
template<class T, unsigned N = 7, class U = double> struct Fixed { int value; };

using Empty = Pack<>;
using Single = Pack<int>;
using Multiple = Pack<int, double>;
using Nested = Pack<Pack<int>, double>;
using MixedEmpty = HeadAndPack<int>;
using Mixed = HeadAndPack<int, double, char>;
using ValueArguments = ValuePack<1, 2, 3>;
using Defaulted = Fixed<int>;
}

//--- module.modulemap
module CxxTemplates {
  header "templates.h"
  requires cplusplus
}

//--- use-templates.swift
import CxxTemplates

public func empty(_ value: test.Empty) -> test.Empty { value }
public func single(_ value: test.Single) -> test.Single { value }
public func multiple(_ value: test.Multiple) -> test.Multiple { value }
public func nested(_ value: test.Nested) -> test.Nested { value }
public func mixedEmpty(_ value: test.MixedEmpty) -> test.MixedEmpty { value }
public func mixed(_ value: test.Mixed) -> test.Mixed { value }
public func nonType(_ value: test.ValueArguments) -> test.ValueArguments { value }
public func defaulted(_ value: test.Defaulted) -> test.Defaulted { value }

// CHECK: SWIFT_INLINE_THUNK test::Fixed<int, 7U, double> defaulted(const test::Fixed<int, 7U, double>& value)
// CHECK: SWIFT_INLINE_THUNK test::Pack<> empty(const test::Pack<>& value)
// CHECK: SWIFT_INLINE_THUNK test::HeadAndPack<int, double, char> mixed(const test::HeadAndPack<int, double, char>& value)
// CHECK: SWIFT_INLINE_THUNK test::HeadAndPack<int> mixedEmpty(const test::HeadAndPack<int>& value)
// CHECK: SWIFT_INLINE_THUNK test::Pack<int, double> multiple(const test::Pack<int, double>& value)
// CHECK: SWIFT_INLINE_THUNK test::Pack<test::Pack<int>, double> nested(const test::Pack<test::Pack<int>, double>& value)
// CHECK: SWIFT_INLINE_THUNK test::ValuePack<1, 2, 3> nonType(const test::ValuePack<1, 2, 3>& value)
// CHECK: SWIFT_INLINE_THUNK test::Pack<int> single(const test::Pack<int>& value)
