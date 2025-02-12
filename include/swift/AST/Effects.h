//===--- Decl.h - Swift Language Declaration ASTs ---------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines some data types used for 'rethrows' and `reasync` checking.
//
// We refer to 'throws' and 'async' as "effects". A function might have either or
// both effects.
//
// A function is _effect polymorphic_ if its effect depends on the call site.
// This can either be unconditional (the usual 'throws' or 'async' case), or it
// can depend on either its arguments or conformances (these are 'rethrows' and
// 'reasync' functions).
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_EFFECTS_H
#define SWIFT_EFFECTS_H

#include "swift/AST/Type.h"
#include "swift/Basic/OptionSet.h"

#include <utility>

namespace llvm {
class raw_ostream;
}

namespace swift {
class AbstractFunctionDecl;
class ProtocolDecl;

enum class EffectKind : uint8_t {
  Throws = 1 << 0,
  Async  = 1 << 1,
  Unsafe = 1 << 2,
};
using PossibleEffects = OptionSet<EffectKind>;

void simple_display(llvm::raw_ostream &out, const EffectKind kind);

class ValueDecl;

class PolymorphicEffectRequirementList {
  using Requirements = ArrayRef<AbstractFunctionDecl *>;
  using Conformances = ArrayRef<std::pair<Type, ProtocolDecl *>>;
private:
  Requirements requirements;
  Conformances conformances;

public:
  PolymorphicEffectRequirementList(Requirements requirements,
                                   Conformances conformances)
    : requirements(requirements), conformances(conformances) {}
  PolymorphicEffectRequirementList() {}

  Requirements getRequirements() const {
    return requirements;
  }

  Conformances getConformances() const {
    return conformances;
  }
};

void simple_display(llvm::raw_ostream &out,
                    const PolymorphicEffectRequirementList reqs);

enum class PolymorphicEffectKind : uint8_t {
  /// The function does not have this effect at all.
  None,

  /// The function has this effect if at least one closure argument has it.
  ///
  /// This is the ordinary 'rethrows' /'reasync' case.
  ByClosure,

  /// The function has this effect if at least one of its conformances has it.
  ///
  /// This is the conformance-based 'rethrows' /'reasync' case.
  ByConformance,

  /// The function is only permitted to be `rethrows` because it depends
  /// on a conformance to `AsyncSequence` or `AsyncIteratorProtocol`,
  /// which historically were "@rethrows" protocols.
  AsyncSequenceRethrows,

  /// The function has this effect unconditionally.
  ///
  /// This is a plain old 'throws' / 'async' function.
  Always,

  /// The function declaration was invalid.
  Invalid
};

void simple_display(llvm::raw_ostream &out, PolymorphicEffectKind value);

} // end namespace swift

#endif
