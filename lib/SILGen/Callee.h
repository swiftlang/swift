//===--- Callee.h -----------------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_SILGEN_CALLEE_H
#define SWIFT_SILGEN_CALLEE_H

#include "swift/AST/ForeignAsyncConvention.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "swift/AST/Types.h"
#include "swift/SIL/AbstractionPattern.h"

namespace swift {
namespace Lowering {

class CalleeTypeInfo {
public:
  Optional<AbstractionPattern> origFormalType;
  CanSILFunctionType substFnType;
  Optional<AbstractionPattern> origResultType;
  CanType substResultType;
  struct ForeignInfo {
    Optional<ForeignErrorConvention> error;
    Optional<ForeignAsyncConvention> async;
    ImportAsMemberStatus self;
  };
  ForeignInfo foreign;

private:
  Optional<SILFunctionTypeRepresentation> overrideRep;

public:
  CalleeTypeInfo() = default;

  CalleeTypeInfo(CanSILFunctionType substFnType,
                 AbstractionPattern origResultType, CanType substResultType,
                 const Optional<ForeignErrorConvention> &foreignError,
                 const Optional<ForeignAsyncConvention> &foreignAsync,
                 ImportAsMemberStatus foreignSelf,
                 Optional<SILFunctionTypeRepresentation> overrideRep = None)
      : origFormalType(llvm::None), substFnType(substFnType),
        origResultType(origResultType),
        substResultType(substResultType), foreign{foreignError, foreignAsync,
                                                  foreignSelf},
        overrideRep(overrideRep) {}

  CalleeTypeInfo(CanSILFunctionType substFnType,
                 AbstractionPattern origResultType, CanType substResultType,
                 Optional<SILFunctionTypeRepresentation> overrideRep = None)
      : origFormalType(llvm::None), substFnType(substFnType),
        origResultType(origResultType), substResultType(substResultType),
        foreign(), overrideRep(overrideRep) {}

  SILFunctionTypeRepresentation getOverrideRep() const {
    return overrideRep.getValueOr(substFnType->getRepresentation());
  }
};

} // namespace Lowering
} // namespace swift

#endif
