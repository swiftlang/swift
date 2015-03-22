//===- Defer.h - 'defer' helper macro ---------------------------*- C++ -*-===//
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
// This filed defines a 'defer' macro for performing a cleanup on any exit out
// of a scope.
//
//===----------------------------------------------------------------------===//

#ifndef __SWIFT_DEFER_H
#define __SWIFT_DEFER_H

namespace swift {
  template <typename F>
  class DoAtScopeExit {
    F &Fn;
    DoAtScopeExit(DoAtScopeExit&) = delete;
    void operator=(DoAtScopeExit&) = delete;
  public:
    DoAtScopeExit(F &Fn) : Fn(Fn){}
    ~DoAtScopeExit() {
      Fn();
    }
  };
}

#define DEFER_CONCAT_IMPL(x, y) x##y
#define DEFER_MACRO_CONCAT(x, y) DEFER_CONCAT_IMPL(x, y)


/// This macro is used to register a function / lambda to be run on exit from a
/// scope.  Its typical use looks like:
///
///   defer([&]{
///     stuff
///   })
///
#define defer(x) \
  auto DEFER_MACRO_CONCAT(defer_func, __LINE__) = (x); \
  swift::DoAtScopeExit<decltype(DEFER_MACRO_CONCAT(defer_func, __LINE__))> \
       DEFER_MACRO_CONCAT(defer_local, __LINE__)\
       (DEFER_MACRO_CONCAT(defer_func, __LINE__));

#endif


