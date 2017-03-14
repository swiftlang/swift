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
class ManagedValue;
class RValue;
class SILGenFunction;

/// An abstract class for working with results.of applies.
class ResultPlan {
public:
  virtual RValue finish(SILGenFunction &SGF, SILLocation loc, CanType substType,
                        ArrayRef<ManagedValue> &directResults) = 0;
  virtual ~ResultPlan() = default;

  virtual void
  gatherIndirectResultAddrs(SmallVectorImpl<SILValue> &outList) const = 0;
};

using ResultPlanPtr = std::unique_ptr<ResultPlan>;

/// The class for building result plans.
struct ResultPlanBuilder {
  SILGenFunction &SGF;
  SILLocation loc;
  ArrayRef<SILResultInfo> allResults;
  SILFunctionTypeRepresentation rep;

  ResultPlanBuilder(SILGenFunction &SGF, SILLocation loc,
                    ArrayRef<SILResultInfo> allResults,
                    SILFunctionTypeRepresentation rep)
      : SGF(SGF), loc(loc), allResults(allResults), rep(rep) {}

  ResultPlanPtr build(Initialization *emitInto, AbstractionPattern origType,
                      CanType substType);
  ResultPlanPtr buildForTuple(Initialization *emitInto,
                              AbstractionPattern origType,
                              CanTupleType substType);

  ~ResultPlanBuilder() {
    assert(allResults.empty() && "didn't consume all results!");
  }
};

} // end namespace Lowering
} // end namespace swift

#endif
