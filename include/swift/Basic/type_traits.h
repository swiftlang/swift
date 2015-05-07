//===--- type_traits.h - Type traits -----------------------------*- C++ -*-==//
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

#ifndef SWIFT_BASIC_TYPE_TRAITS_H
#define SWIFT_BASIC_TYPE_TRAITS_H

#include <type_traits>

#ifndef __has_feature
#define SWIFT_DEFINED_HAS_FEATURE
#define __has_feature(x) 0
#endif

namespace swift {

/// Same as \c std::is_trivially_copyable, which we can not use directly
/// because it is not implemented yet in all C++11 standard libraries.
///
/// Unlike \c llvm::isPodLike, this trait should produce a precise result and
/// is not intended to be specialized.
template<typename T>
struct IsTriviallyCopyable {
#if _LIBCPP_VERSION
  // libc++ implements it.
  static const bool value = std::is_trivially_copyable<T>::value;
#elif __has_feature(is_trivially_copyable)
  static const bool value = __is_trivially_copyable(T);
#else
#  error "Not implemented"
#endif
};

template<typename T>
struct IsTriviallyConstructible {
#if _LIBCPP_VERSION
  // libc++ implements it.
  static const bool value = std::is_trivially_constructible<T>::value;
#elif __has_feature(has_trivial_constructor)
  static const bool value = __has_trivial_constructor(T);
#else
#  error "Not implemented"
#endif
};

template<typename T>
struct IsTriviallyDestructible {
#if _LIBCPP_VERSION
  // libc++ implements it.
  static const bool value = std::is_trivially_destructible<T>::value;
#elif __has_feature(has_trivial_destructor)
  static const bool value = __has_trivial_destructor(T);
#else
#  error "Not implemented"
#endif
};

} // namespace swift

#ifdef SWIFT_DEFINED_HAS_FEATURE
#undef __has_feature
#endif

#endif // SWIFT_BASIC_TYPE_TRAITS_H

