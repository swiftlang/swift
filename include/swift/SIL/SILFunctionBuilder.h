//===--- SILFunctionBuilder.h ---------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILFUNCTIONBUILDER_H
#define SWIFT_SIL_SILFUNCTIONBUILDER_H

#include "swift/SIL/SILModule.h"

namespace swift {

class SILParserFunctionBuilder;
class SILSerializationFunctionBuilder;
class SILOptFunctionBuilder;
namespace Lowering {
class SILGenFunctionBuilder;
} // namespace Lowering

/// A class for creating SILFunctions in a specific SILModule.
///
/// The intention is that this class is not used directly, but rather that each
/// part of the compiler that needs to create functions create a composition
/// type with SILFunctionBuilder as a field. This enables subsystems that use
/// SIL to:
///
/// 1. Enforce invariants in the type system. An example of this is in the
///    SILOptimizer where we want to ensure that the pass manager properly
///    notifies analyses whenever functions are created/destroyed.
///
/// 2. Have a convenient place to place utility functionality for creating
///    functions. Today the compiler has many small utility functions for
///    creating the underlying SILFunction that are generally quite verbose and
///    have shared code. These SILFunctionBuilder composition types will enable
///    code-reuse in between these different SILFunction creation sites.
class SILFunctionBuilder {
  SILModule &mod;

  friend class SILParserFunctionBuilder;
  friend class SILSerializationFunctionBuilder;
  friend class SILOptFunctionBuilder;
  friend class Lowering::SILGenFunctionBuilder;

  SILFunctionBuilder(SILModule &mod) : mod(mod) {}

  /// Return the declaration of a utility function that can, but needn't, be
  /// shared between different parts of a program.
  SILFunction *getOrCreateSharedFunction(SILLocation loc, StringRef name,
                                         CanSILFunctionType type,
                                         IsBare_t isBareSILFunction,
                                         IsTransparent_t isTransparent,
                                         IsSerialized_t isSerialized,
                                         ProfileCounter entryCount,
                                         IsThunk_t isThunk);

  /// Return the declaration of a function, or create it if it doesn't exist.
  SILFunction *getOrCreateFunction(
      SILLocation loc, StringRef name, SILLinkage linkage,
      CanSILFunctionType type, IsBare_t isBareSILFunction,
      IsTransparent_t isTransparent, IsSerialized_t isSerialized,
      ProfileCounter entryCount = ProfileCounter(),
      IsThunk_t isThunk = IsNotThunk,
      SubclassScope subclassScope = SubclassScope::NotApplicable);

  /// Return the declaration of a function, or create it if it doesn't exist.
  SILFunction *
  getOrCreateFunction(SILLocation loc, SILDeclRef constant,
                      ForDefinition_t forDefinition,
                      ProfileCounter entryCount = ProfileCounter());

  /// Create a function declaration.
  ///
  /// This signature is a direct copy of the signature of SILFunction::create()
  /// in order to simplify refactoring all SILFunction creation use-sites to use
  /// SILFunctionBuilder. Eventually the uses should probably be refactored.
  SILFunction *
  createFunction(SILLinkage linkage, StringRef name,
                 CanSILFunctionType loweredType, GenericEnvironment *genericEnv,
                 Optional<SILLocation> loc, IsBare_t isBareSILFunction,
                 IsTransparent_t isTrans, IsSerialized_t isSerialized,
                 ProfileCounter entryCount = ProfileCounter(),
                 IsThunk_t isThunk = IsNotThunk,
                 SubclassScope subclassScope = SubclassScope::NotApplicable,
                 Inline_t inlineStrategy = InlineDefault,
                 EffectsKind EK = EffectsKind::Unspecified,
                 SILFunction *InsertBefore = nullptr,
                 const SILDebugScope *DebugScope = nullptr);
};
} // namespace swift

#endif
