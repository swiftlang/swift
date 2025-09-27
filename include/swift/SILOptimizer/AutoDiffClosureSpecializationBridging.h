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

#include "swift/SIL/SILBridging.h"
#include <unordered_map>
#include <vector>

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

struct BridgedTypeHasher {
  unsigned operator()(const BridgedType &value) const;
};

SWIFT_IMPORT_UNSAFE bool operator==(const BridgedType &lhs,
                                    const BridgedType &rhs);

using SpecializedBranchTracingEnumDict =
    std::unordered_map<BridgedType, BridgedType, BridgedTypeHasher>;

struct ClosureAndIdxInPayload {
  SWIFT_IMPORT_UNSAFE ClosureAndIdxInPayload(BridgedInstruction closure,
                                             SwiftInt idxInPayload);
  BridgedInstruction closure;
  SwiftInt idxInPayload;
};

using VectorOfClosureAndIdxInPayload = std::vector<ClosureAndIdxInPayload>;

struct BranchTracingEnumAndClosureInfo {
  BridgedType enumType;
  SwiftInt enumCaseIdx;
  BridgedInstruction closure;
  SwiftInt idxInPayload;
};

using VectorOfBranchTracingEnumAndClosureInfo =
    std::vector<BranchTracingEnumAndClosureInfo>;

SWIFT_IMPORT_UNSAFE SpecializedBranchTracingEnumDict
autodiffSpecializeBranchTracingEnums(
    BridgedFunction topVJP, BridgedType topBTE,
    const VectorOfBranchTracingEnumAndClosureInfo
        &vectorOfBranchTracingEnumAndClosureInfo);

SWIFT_IMPORT_UNSAFE BridgedArgument specializeBranchTracingEnumBBArgInVJP(
    BridgedArgument arg, const SpecializedBranchTracingEnumDict &specBTEDict);

SWIFT_IMPORT_UNSAFE BridgedArgument specializePayloadTupleBBArgInPullback(
    BridgedArgument arg, BridgedType enumType, SwiftInt caseIdx);

SWIFT_IMPORT_UNSAFE BridgedOwnedString
getSpecializedBranchTracingEnumDictAsString(
    const SpecializedBranchTracingEnumDict &specBTEDict);

SWIFT_END_NULLABILITY_ANNOTATIONS

#endif
