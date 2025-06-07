//===----------------------------------------------------------------------===//
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

#ifndef SWIFT_REFACTORING_RENAMELOC_H
#define SWIFT_REFACTORING_RENAMELOC_H

#include "swift/Basic/LLVM.h"

namespace swift {
namespace ide {

/// Describes how a `ResolvedLoc` is being used
enum class RenameLocUsage {
  /// The definition of a function/subscript/variable/...
  Definition,

  /// The symbol is being referenced.
  ///
  /// This includes
  ///  - References to variables
  ///  - Unapplied references to functions (`myStruct.memberFunc`)
  ///  - Calls to subscripts (`myArray[1]`, location is `[` here, length 1)
  Reference,

  /// A function that is being called.
  Call,

  /// Unknown name usage occurs if we don't have an entry in the index that
  /// tells us whether the location is a call, reference or a definition. The
  /// most common reasons why this happens is if the editor is adding syntactic
  /// results (eg. from comments or string literals).
  Unknown
};

/// The input to `findSyntacticRenameRanges`.
///
/// Specifies the location of a base name for which `findSyntacticRenameRanges`
/// should find the argument labels as well as some semantic information needed
/// to resolve the rename ranges.
struct RenameLoc {
  /// The line at which the base name starts (1-based).
  unsigned Line;

  /// The column at which the base name (excluding trivia) starts (1-based).
  unsigned Column;

  /// The offset at which the related name starts.
  unsigned Offset;

  /// The length of the base name in the related identifier. For functions,
  /// this does not include the parameters/arguments.
  unsigned Length;

  /// How the identifier is being used (call, reference, definition, unknown).
  ///
  /// Used to decide whether a given occurance should be renamed and/or if its
  /// argument labels should be renamed.
  RenameLocUsage Usage;

  /// The old decl name being renamed.
  ///
  /// ### Examples
  /// - `foo(a:b:)` for a function with two parameters.
  /// - `foo` for a variable.
  /// - `foo(_:)` for a function with a single unnamed parameter
  /// - `foo()` for a function without parameters.
  StringRef OldName;

  RenameLoc(unsigned Line, unsigned Column, RenameLocUsage Usage,
            StringRef OldName)
      : Line(Line), Column(Column), Usage(Usage), OldName(OldName) {}
};

} // namespace ide
} // namespace swift

#endif // SWIFT_REFACTORING_RENAMELOC_H
