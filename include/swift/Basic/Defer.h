//===--- Defer.h - 'defer' helper macro -------------------------*- C++ -*-===//
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
// This file defines a 'SWIFT_DEFER' macro for performing a cleanup on any exit
// out of a scope.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_DEFER_H
#define SWIFT_BASIC_DEFER_H

#include <type_traits>

namespace swift {
  template <typename F>
  class DoAtScopeExit {
    F &Fn;
    void operator=(DoAtScopeExit&) = delete;
  public:
    DoAtScopeExit(F &Fn) : Fn(Fn){}
    ~DoAtScopeExit() {
      Fn();
    }
  };

  namespace detail {
    struct DeferTask {};
    template<typename F>
    DoAtScopeExit<typename std::decay<F>::type> operator+(DeferTask, F&& fn) {
      return DoAtScopeExit<typename std::decay<F>::type>(fn);
    }
  }
} // end namespace swift


#define DEFER_CONCAT_IMPL(x, y) x##y
#define DEFER_MACRO_CONCAT(x, y) DEFER_CONCAT_IMPL(x, y)

/// This macro is used to register a function / lambda to be run on exit from a
/// scope.  Its typical use looks like:
///
///   SWIFT_DEFER {
///     stuff
///   };
///
#define SWIFT_DEFER                                                            \
  auto DEFER_MACRO_CONCAT(defer_func, __COUNTER__) =                           \
      ::swift::detail::DeferTask() + [&]()

#endif // SWIFT_BASIC_DEFER_H
