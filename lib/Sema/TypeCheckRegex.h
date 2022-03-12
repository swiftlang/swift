//===--- TypeCheckRegex.h - Regex type checking utilities -----------------===//
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

#ifndef SWIFT_TYPE_CHECK_REGEX_H
#define SWIFT_TYPE_CHECK_REGEX_H

#include <cstdint>
#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/SmallVector.h>

namespace swift {

class ASTContext;
class TupleTypeElt;
class Type;

enum class RegexCaptureStructureCode: uint8_t {
  End           = 0,
  Atom          = 1,
  NamedAtom     = 2,
  FormArray     = 3,
  FormOptional  = 4,
  BeginTuple    = 5,
  EndTuple      = 6,
  CaseCount
};

/// Decodes regex capture types from the given serialization and appends the
/// decoded capture types to @p result. Returns true if the serialization is
/// malformed.
bool decodeRegexCaptureTypes(ASTContext &ctx,
                             llvm::ArrayRef<uint8_t> serialization,
                             Type atomType,
                             llvm::SmallVectorImpl<TupleTypeElt> &result);

} // end namespace swift

#endif // SWIFT_TYPE_CHECK_REGEX_H
