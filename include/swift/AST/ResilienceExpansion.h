//===--- ResilienceExpansion.h ----------------------------------*- C++ -*-===//
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

#ifndef SWIFT_AST_RESILIENCE_EXPANSION_H
#define SWIFT_AST_RESILIENCE_EXPANSION_H

#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/ErrorHandling.h"

namespace swift {

/// A specification for how much to expand resilient types.
///
/// Right now, this is just a placeholder; a proper expansion
/// specification will probably need to be able to express things like
/// 'expand any type that was fragile in at least such-and-such
/// version'.
enum class ResilienceExpansion : unsigned {
  /// A minimal expansion does not expand types that do not have a
  /// universally fragile representation.  This provides a baseline
  /// for what all components can possibly support.
  ///   - All exported functions must be compiled to at least provide
  ///     a minimally-expanded entrypoint, or else it will be
  ///     impossible for components that do not have that type
  ///     to call the function.
  ///   - Similarly, any sort of abstracted function call must go through
  ///     a minimally-expanded entrypoint.
  ///
  /// Minimal expansion will generally pass all resilient types indirectly.
  Minimal,

  /// A maximal expansion expands all types with fragile
  /// representation, even when they're not universally fragile.  This
  /// is better when internally manipulating values or when working
  /// with specialized entry points for a function.
  Maximal,

  Last_ResilienceExpansion = Maximal
};

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                                     ResilienceExpansion expansion) {
  switch (expansion) {
  case ResilienceExpansion::Minimal:
    return os << "Minimal";
  case ResilienceExpansion::Maximal:
    return os << "Maximal";
  }
  llvm_unreachable("Unhandled ResilienceExpansion in switch");
}

} // namespace swift

#endif // LLVM_SWIFT_AST_CAPTURE_INFO_H

