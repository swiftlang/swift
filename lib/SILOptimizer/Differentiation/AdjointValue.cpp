//===--- AdjointValue.h - Helper class for differentiation ----*- C++ -*---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// AdjointValue - a symbolic representation for adjoint values enabling
// efficient differentiation by avoiding zero materialization.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "differentiation"

#include "swift/SILOptimizer/Differentiation/AdjointValue.h"

void swift::autodiff::AdjointValue::print(llvm::raw_ostream &s) const {
  switch (getKind()) {
  case AdjointValueKind::Zero:
    s << "Zero[" << getType() << ']';
    break;
  case AdjointValueKind::Aggregate:
    s << "Aggregate[" << getType() << "](";
    if (auto *decl = getType().getASTType()->getStructOrBoundGenericStruct()) {
      interleave(
          llvm::zip(decl->getStoredProperties(), getAggregateElements()),
          [&s](std::tuple<VarDecl *, const AdjointValue &> elt) {
            s << std::get<0>(elt)->getName() << ": ";
            std::get<1>(elt).print(s);
          },
          [&s] { s << ", "; });
    } else if (getType().is<TupleType>()) {
      interleave(
          getAggregateElements(),
          [&s](const AdjointValue &elt) { elt.print(s); }, [&s] { s << ", "; });
    } else {
      llvm_unreachable("Invalid aggregate");
    }
    s << ')';
    break;
  case AdjointValueKind::Concrete:
    s << "Concrete[" << getType() << "](" << base->value.concrete << ')';
    break;
  case AdjointValueKind::AddElement:
    auto *addElementValue = getAddElementValue();
    auto baseAdjoint = addElementValue->baseAdjoint;
    auto eltToAdd = addElementValue->eltToAdd;

    s << "AddElement[";
    baseAdjoint.print(s);

    s << ", Field(";
    if (addElementValue->isTupleAdjoint()) {
      s << addElementValue->getFieldIndex();
    } else {
      s << addElementValue->getFieldDecl()->getNameStr();
    }
    s << "), ";

    eltToAdd.print(s);

    s << "]";
    break;
  }
}
