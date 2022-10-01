//===--- chrono_utils.h - Utility functions for duration ------ -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Specifically, we want ceil() for these types, but that's only available in
// C++17, and we need to build with C++14, so... include a version of the
// necesary code here.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_THREADING_IMPL_CHRONO_UTILS_H
#define SWIFT_THREADING_IMPL_CHRONO_UTILS_H

#include <chrono>
#include <type_traits>

namespace swift {
namespace threading_impl {
namespace chrono_utils {

#if __cplusplus >= 201703L
using std::chrono::ceil;
#else

namespace detail {
template <class _Tp>
struct is_duration : std::false_type {};

template <class _Rep, class _Period>
struct is_duration<std::chrono::duration<_Rep, _Period> >
  : std::true_type  {};

template <class _Rep, class _Period>
struct is_duration<const std::chrono::duration<_Rep, _Period> >
  : std::true_type  {};

template <class _Rep, class _Period>
struct is_duration<volatile std::chrono::duration<_Rep, _Period> >
  : std::true_type  {};

template <class _Rep, class _Period>
struct is_duration<const volatile std::chrono::duration<_Rep, _Period> >
  : std::true_type  {};
}

template <class To, class Rep, class Period,
          class = std::enable_if_t<detail::is_duration<To>::value>>
constexpr To
ceil(const std::chrono::duration<Rep, Period>& d)
{
  To t = std::chrono::duration_cast<To>(d);
  if (t < d)
    t = t + To{1};
  return t;
}

#endif

} // namespace chrono_utils
} // namespace threading_impl
} // namespace swift

#endif // SWIFT_THREADING_IMPL_CHRONO_UTILS_H
