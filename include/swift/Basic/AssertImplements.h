//===--- AssertImplements.h - Assert that a class overrides a function ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file provides the ASSERT_IMPLEMENTS macro, which statically
//  asserts that a class implements a function.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_ASSERTIMPLEMENTS_H
#define SWIFT_BASIC_ASSERTIMPLEMENTS_H

#include <type_traits>

namespace swift {
  template <class ExpectedType, class BaseType>
  BaseType get_implementing_class_helper(ExpectedType BaseType::*member);

#define GET_IMPLEMENTING_CLASS(DerivedClass, MemberName, ExpectedType)          \
  decltype(::swift::get_implementing_class_helper<ExpectedType>(                \
                                                    &DerivedClass::MemberName))

/// Statically assert that DerivedClass overrides the instance method
/// MemberName (at overload ExpectedType).  It doesn't have to be
/// overridden directly in DerivedClass; it just can't still be the
/// implementation from BaseClass.
#define ASSERT_IMPLEMENTS(DerivedClass, BaseClass, MemberName, ExpectedType)    \
  static_assert(!::std::is_same<BaseClass,                                      \
                                GET_IMPLEMENTING_CLASS(DerivedClass, MemberName,\
                                                       ExpectedType)            \
                               >::value,                                        \
                "" #DerivedClass " does not properly override " #MemberName)


  template <class T, T *impl0, T *impl1> struct is_same_pointer {
    enum { value = false };
  };

  template <class T, T *impl> struct is_same_pointer<T, impl, impl> {
    enum { value = true };
  };

/// Statically assert that DerivedClass overrides the static method
/// MemberName (at overload ExpectedType).  It doesn't have to be
/// overridden directly in DerivedClass; it just can't still be the
/// implementation from BaseClass.
#define ASSERT_IMPLEMENTS_STATIC(DerivedClass, BaseClass, MemberName,           \
                                 ExpectedType)                                  \
  static_assert(!::swift::is_same_pointer<ExpectedType,                         \
                                          &BaseClass::MemberName,               \
                                          &DerivedClass::MemberName>::value,    \
                "" #DerivedClass " does not properly override " #MemberName)

} // end namespace swift

#endif // SWIFT_BASIC_ASSERTIMPLEMENTS_H
