//===------- DifferentiationMangler.h --------- differentiation -*- C++ -*-===//
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

#ifndef SWIFT_SIL_UTILS_DIFFERENTIATIONMANGLER_H
#define SWIFT_SIL_UTILS_DIFFERENTIATIONMANGLER_H

#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/AutoDiff.h"
#include "swift/Basic/NullablePtr.h"
#include "swift/Demangling/Demangler.h"
#include "swift/SIL/SILFunction.h"

namespace swift {
namespace Mangle {

/// A mangler for generated differentiation functions.
class DifferentiationMangler : public ASTMangler {
public:
  DifferentiationMangler(const ASTContext &Ctx) : ASTMangler(Ctx) {}
  /// Returns the mangled name for a differentiation function of the given kind.
  std::string mangleAutoDiffFunction(StringRef originalName,
                                     Demangle::AutoDiffFunctionKind kind,
                                     const AutoDiffConfig &config);
  /// Returns the mangled name for a derivative function of the given kind.
  std::string mangleDerivativeFunction(StringRef originalName,
                                       AutoDiffDerivativeFunctionKind kind,
                                       const AutoDiffConfig &config);
  /// Returns the mangled name for a linear map of the given kind.
  std::string mangleLinearMap(StringRef originalName,
                              AutoDiffLinearMapKind kind,
                              const AutoDiffConfig &config);
  /// Returns the mangled name for a derivative function subset parameters
  /// thunk.
  std::string mangleDerivativeFunctionSubsetParametersThunk(
      StringRef originalName, CanType toType,
      AutoDiffDerivativeFunctionKind linearMapKind,
      IndexSubset *fromParamIndices, IndexSubset *fromResultIndices,
      IndexSubset *toParamIndices);
  /// Returns the mangled name for a linear map subset parameters thunk.
  std::string mangleLinearMapSubsetParametersThunk(
      CanType fromType, AutoDiffLinearMapKind linearMapKind,
      IndexSubset *fromParamIndices, IndexSubset *fromResultIndices,
      IndexSubset *toParamIndices);
};

} // end namespace Mangle
} // end namespace swift

#endif /* SWIFT_SIL_UTILS_DIFFERENTIATIONMANGLER_H */
