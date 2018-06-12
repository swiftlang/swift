//===--- SimpleDisplay.h - Simple Type Display ------------------*- C++ -*-===//
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
//  This file defines the main customization point, simple_display, for
//  displaying values of a given type
//  encoding of (static) type information for use as a simple replacement
//  for run-time type information.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_BASIC_SIMPLE_DISPLAY_H
#define SWIFT_BASIC_SIMPLE_DISPLAY_H

#include "llvm/Support/raw_ostream.h"
#include <tuple>
#include <type_traits>

namespace swift {
  template<typename T>
  void simple_display(llvm::raw_ostream &out, const T &value) {
    out << value;
  }

  template<unsigned I, typename ...Types,
  typename std::enable_if<I == sizeof...(Types)>::type* = nullptr>
  void simple_display_tuple(llvm::raw_ostream &out,
                            const std::tuple<Types...> &value);

  template<unsigned I, typename ...Types,
           typename std::enable_if<I < sizeof...(Types)>::type* = nullptr>
  void simple_display_tuple(llvm::raw_ostream &out,
                            const std::tuple<Types...> &value) {
    // Start or separator.
    if (I == 0) out << "(";
    else out << ", ";

    // Current element.
    simple_display(out, std::get<I>(value));

    // Print the remaining elements.
    simple_display_tuple<I+1>(out, value);
  }

  template<unsigned I, typename ...Types,
           typename std::enable_if<I == sizeof...(Types)>::type*>
  void simple_display_tuple(llvm::raw_ostream &out,
                            const std::tuple<Types...> &value) {
    // Last element.
    out << ")";
  }

  template<typename ...Types>
  void simple_display(llvm::raw_ostream &out,
                      const std::tuple<Types...> &value) {
    simple_display_tuple<0>(out, value);
  }
}

#endif // SWIFT_BASIC_SIMPLE_DISPLAY_H
