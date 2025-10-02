//===--- AutoDiffClosureSpecializationBridging.h --------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_ADCSBRIDGING_H
#define SWIFT_SILOPTIMIZER_ADCSBRIDGING_H

#include "swift/AST/ASTBridging.h"
#include "swift/Basic/BasicBridging.h"
#include "swift/SIL/SILBridging.h"

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

SWIFT_IMPORT_UNSAFE BridgedType
getBranchingTraceEnumLoweredType(BridgedDeclObj ed, BridgedFunction vjp);

SWIFT_IMPORT_UNSAFE BridgedType
getBranchingTraceEnumLoweredType(BridgedEnumDecl ed, BridgedFunction vjp);

SWIFT_IMPORT_UNSAFE BridgedNullableGenericParamList
cloneGenericParameters(BridgedASTContext ctx, BridgedDeclContext dc,
                       BridgedCanGenericSignature sig);

SWIFT_IMPORT_UNSAFE BridgedSourceFile autodiffGetSourceFile(BridgedFunction f);

SWIFT_IMPORT_UNSAFE BridgedOwnedString getEnumDeclAsString(BridgedType bteType);

SWIFT_END_NULLABILITY_ANNOTATIONS

#endif
