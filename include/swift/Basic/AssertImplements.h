//===- AssertImplements.h - Assert that a class overrides a function ------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file provides the ASSERT_IMPLEMENTS macro, which statically
//  asserts that a class implements a function.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_ASSERT_IMPLEMENTS_H
#define SWIFT_BASIC_ASSERT_IMPLEMENTS_H

#include <type_traits>

namespace swift {
  template <class ExpectedMemberType, class BaseType>
  BaseType get_implementing_class_helper(ExpectedMemberType BaseType::*member);

#define GET_IMPLEMENTING_CLASS(DerivedClass, MemberName, ExpectedType)          \
  decltype(::swift::get_implementing_class_helper<ExpectedType>(                \
                                                    &DerivedClass::MemberName))

#define ASSERT_IMPLEMENTS(DerivedClass, BaseClass, MemberName, ExpectedType)    \
  static_assert(!::std::is_same<BaseClass,                                      \
                                GET_IMPLEMENTING_CLASS(DerivedClass, MemberName,\
                                                       ExpectedType)            \
                               >::value,                                        \
                "" #DerivedClass " does not properly override " #MemberName)
}

#endif
