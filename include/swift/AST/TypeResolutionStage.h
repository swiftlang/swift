//===--- TypeResolutionStage.h - Type Resolution Stage ----------*- C++ -*-===//
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
#ifndef SWIFT_AST_TYPE_RESOLUTION_STAGE_H
#define SWIFT_AST_TYPE_RESOLUTION_STAGE_H

namespace llvm {
class raw_ostream;
}

namespace swift {

/// Describes the stage at which a particular type should be computed.
///
/// Later stages compute more information about the type, requiring more
/// complete analysis.
enum class TypeResolutionStage : uint8_t {
  /// Produces an interface type describing its structure, but without
  /// performing semantic analysis to resolve (e.g.) references to members of
  /// type parameters.
  Structural,

  /// Produces a complete interface type where all member references have been
  /// resolved.
  Interface,

  /// Produces a contextual type involving archetypes within the context of
  /// the type.
  Contextual,
};

/// Display a type resolution stage.
void simple_display(llvm::raw_ostream &out, const TypeResolutionStage &value);

} // end namespace swift

#endif /* SWIFT_AST_TYPE_RESOLUTION_STAGE_H */
