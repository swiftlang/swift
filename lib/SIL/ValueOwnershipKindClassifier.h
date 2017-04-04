//===--- ValueOwnershipKindClassifier.h -----------------------------------===//
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

#ifndef SWIFT_SIL_VALUEOWNERSHIPKINDCLASSIFIER_H
#define SWIFT_SIL_VALUEOWNERSHIPKINDCLASSIFIER_H

#include "swift/SIL/SILVisitor.h"

namespace swift {
namespace sil {

class ValueOwnershipKindClassifier
    : public SILVisitor<ValueOwnershipKindClassifier, ValueOwnershipKind> {

public:
  ValueOwnershipKindClassifier() = default;
  ~ValueOwnershipKindClassifier() = default;
  ValueOwnershipKindClassifier(const ValueOwnershipKindClassifier &) = delete;
  ValueOwnershipKindClassifier(ValueOwnershipKindClassifier &&) = delete;

  ValueOwnershipKind visitForwardingInst(SILInstruction *I,
                                         ArrayRef<Operand> Ops);
  ValueOwnershipKind visitForwardingInst(SILInstruction *I) {
    return visitForwardingInst(I, I->getAllOperands());
  }

  ValueOwnershipKind visitValueBase(ValueBase *V) {
    llvm_unreachable("unimplemented method on ValueBaseOwnershipVisitor");
  }
#define VALUE(Id, Parent) ValueOwnershipKind visit##Id(Id *ID);
#include "swift/SIL/SILNodes.def"
};

} // end namespace sil
} // end namespace swift

#endif
