// rdar://125745834 (https://github.com/swiftlang/swift/issues/72737)
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -I %t/Inputs %t/main.swift -cxx-interoperability-mode=default

//--- Inputs/module.modulemap
module CxxMod {
  header "test.h"
  requires cplusplus
}

//--- Inputs/test.h
#pragma once

namespace ns {
inline namespace v1 {
namespace chrono {
struct steady_clock {
  struct time_point {
    long value;
  };
  static time_point now() { return {0}; }
};
}
}
}

//--- main.swift
import CxxMod

let n = ns.chrono.steady_clock.now() // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
