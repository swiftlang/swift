//===--- DerivativeLookup.h -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// SWIFT_ENABLE_TENSORFLOW
//
// Utilities for looking up derivatives of functions.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_MANDATORY_DIFFERENTIATION_DERIVATIVELOOKUP_H
#define SWIFT_SILOPTIMIZER_MANDATORY_DIFFERENTIATION_DERIVATIVELOOKUP_H

#include "swift/AST/AutoDiff.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILModule.h"

namespace swift {

/// Returns a differentiability witness (definition or declaration) exactly
/// matching the specified indices. If none are found in the given `module`,
/// returns `nullptr`.
///
/// \param parameterIndices must be lowered to SIL.
/// \param resultIndices must be lowered to SIL.
SILDifferentiabilityWitness *
getExactDifferentiabilityWitness(SILModule &module, SILFunction *original,
                                 IndexSubset *parameterIndices,
                                 IndexSubset *resultIndices);

/// Finds the "@differentiable" attribute on `original` whose parameter indices
/// are a minimal superset of the specified parameter indices. Returns `nullptr`
/// if no such attribute exists.
///
/// \param parameterIndices must be lowered to SIL.
/// \param minimalParameterIndices is an output parameter that is set to the SIL
/// indices of the minimal attribute, or to `nullptr` if no attribute exists.
const DifferentiableAttr *
getMinimalASTDifferentiableAttr(AbstractFunctionDecl *original,
                                IndexSubset *parameterIndices,
                                IndexSubset *&minimalParameterIndices);

/// Returns a differentiability witness for `original` whose parameter indices
/// are a minimal superset of the specified parameter indices and whose result
/// indices match the given result indices, out of all
/// differentiability witnesses that come from AST "@differentiable" or
/// "@differentiating" attributes.
///
/// This function never creates new differentiability witness definitions.
/// However, this function may create new differentiability witness declarations
/// referring to definitions in other modules when these witnesses have not yet
/// been declared in the current module.
///
/// \param module is the SILModule in which to get or create the witnesses.
/// \param parameterIndices must be lowered to SIL.
/// \param resultIndices must be lowered to SIL.
SILDifferentiabilityWitness *getOrCreateMinimalASTDifferentiabilityWitness(
    SILModule &module, SILFunction *original, IndexSubset *parameterIndices,
    IndexSubset *resultIndices);

} // end namespace swift

#endif // SWIFT_SILOPTIMIZER_MANDATORY_DIFFERENTIATION_DERIVATIVELOOKUP_H
