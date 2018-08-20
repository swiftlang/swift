//===- tapi/Core/STLExtras.h - STL Extras -----------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief Simplify common uses cases of C++ code.
///
//===----------------------------------------------------------------------===//

#ifndef TAPI_CORE_STL_EXTRAS_H
#define TAPI_CORE_STL_EXTRAS_H

#include "Defines.h"
#include <algorithm>

TAPI_NAMESPACE_INTERNAL_BEGIN

template <class _T, class _Compare>
inline bool is_sorted(const _T &container, _Compare cmp) {
  return std::is_sorted(std::begin(container), std::end(container), cmp);
}

template <class _T> inline bool is_sorted(const _T &container) {
  return std::is_sorted(std::begin(container), std::end(container));
}

template <class _T, class _Compare>
inline void sort(_T &container, _Compare cmp) {
  std::sort(std::begin(container), std::end(container), cmp);
}

template <class _T> inline void sort(_T &container) {
  std::sort(std::begin(container), std::end(container));
}

template <class _T, class _Tp>
inline auto find(_T &container, const _Tp &value)
    -> decltype(std::find(std::begin(container), std::end(container), value)) {
  return std::find(std::begin(container), std::end(container), value);
}

template <class _T, class _Predicate>
inline auto find_if(_T &container, _Predicate pred)
    -> decltype(std::find_if(std::begin(container), std::end(container),
                             pred)) {
  return std::find_if(std::begin(container), std::end(container), pred);
}

template <class _T, class _Predicate>
inline auto remove_if(_T &container, _Predicate pred)
    -> decltype(std::remove_if(std::begin(container), std::end(container),
                               pred)) {
  return std::remove_if(std::begin(container), std::end(container), pred);
}

template <class _T, class _Tp, class _Compare>
inline auto lower_bound(_T &container, const _Tp &value, _Compare cmp)
    -> decltype(std::lower_bound(std::begin(container), std::end(container),
                                 value, cmp)) {
  return std::lower_bound(std::begin(container), std::end(container), value,
                          cmp);
}

template <class _T1, class _T2>
inline bool equal(const _T1 &container1, const _T2 &container2) {
  return std::equal(std::begin(container1), std::end(container1),
                    std::begin(container2));
}

TAPI_NAMESPACE_INTERNAL_END

#endif // TAPI_CORE_STL_EXTRAS_H
