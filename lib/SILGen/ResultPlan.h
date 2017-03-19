//===--- ResultPlan.h -----------------------------------------------------===//
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

#ifndef SWIFT_SILGEN_RESULTPLAN_H
#define SWIFT_SILGEN_RESULTPLAN_H

#include "Callee.h"
#include "ManagedValue.h"
#include "swift/AST/Types.h"
#include "swift/Basic/LLVM.h"
#include "swift/SIL/SILLocation.h"
#include <memory>

namespace swift {

class CanType;
class SILValue;

namespace Lowering {

class AbstractionPattern;
class Initialization;
class RValue;
class SILGenFunction;
class SGFContext;
class CalleeTypeInfo;

/// An abstract class for working with results.of applies.
class ResultPlan {
public:
  virtual RValue finish(SILGenFunction &SGF, SILLocation loc, CanType substType,
                        ArrayRef<ManagedValue> &directResults) = 0;
  virtual ~ResultPlan() = default;

  virtual void
  gatherIndirectResultAddrs(SmallVectorImpl<SILValue> &outList) const = 0;

  virtual Optional<std::pair<ManagedValue, ManagedValue>>
  emitForeignErrorArgument(SILGenFunction &SGF, SILLocation loc) {
    return None;
  }
};

using ResultPlanPtr = std::unique_ptr<ResultPlan>;

/// The class for building result plans.
struct ResultPlanBuilder {
  SILGenFunction &SGF;
  SILLocation loc;
  const CalleeTypeInfo &calleeTypeInfo;
  ArrayRef<SILResultInfo> allResults;

  ResultPlanBuilder(SILGenFunction &SGF, SILLocation loc,
                    const CalleeTypeInfo &calleeTypeInfo)
      : SGF(SGF), loc(loc), calleeTypeInfo(calleeTypeInfo),
        allResults(calleeTypeInfo.substFnType->getResults()) {}

  ResultPlanPtr build(Initialization *emitInto, AbstractionPattern origType,
                      CanType substType);
  ResultPlanPtr buildForTuple(Initialization *emitInto,
                              AbstractionPattern origType,
                              CanTupleType substType);

  static ResultPlanPtr computeResultPlan(SILGenFunction &SGF,
                                         const CalleeTypeInfo &calleeTypeInfo,
                                         SILLocation loc,
                                         SGFContext evalContext);

  ~ResultPlanBuilder() {
    assert(allResults.empty() && "didn't consume all results!");
  }

private:
  ResultPlanPtr buildTopLevelResult(Initialization *init, SILLocation loc);
};

} // end namespace Lowering
} // end namespace swift

#endif
