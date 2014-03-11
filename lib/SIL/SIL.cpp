//===--- SIL.cpp - Implements random SIL functionality --------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/FormalLinkage.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/SILUndef.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/AnyFunctionRef.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Mangle.h"
#include "swift/AST/Pattern.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Basic/Fallthrough.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"
using namespace swift;

void ValueBase::replaceAllUsesWith(ValueBase *RHS) {
  assert(this != RHS && "Cannot RAUW a value with itself");
  assert(getNumTypes() == RHS->getNumTypes() &&
         "An instruction and the value base that it is being replaced by "
         "must have the same number of types");

  while (!use_empty()) {
    Operand *Op = *use_begin();
    Op->set(SILValue(RHS, Op->get().getResultNumber()));
  }
}


SILUndef *SILUndef::get(SILType Ty, SILModule *M) {
  // Unique these.
  SILUndef *&Entry = M->UndefValues[Ty];
  if (Entry == nullptr)
    Entry = new (*M) SILUndef(Ty);
  return Entry;
}

static FormalLinkage getGenericClauseLinkage(ArrayRef<GenericParam> params) {
  FormalLinkage result = FormalLinkage::Top;
  for (auto &param : params) {
    for (auto proto : param.getAsTypeParam()->getProtocols())
      result ^= getTypeLinkage(CanType(proto->getDeclaredType()));
    if (auto superclass = param.getAsTypeParam()->getSuperclass())
      result ^= getTypeLinkage(superclass->getCanonicalType());
  }
  return result;
}

FormalLinkage swift::getDeclLinkage(Decl *D) {
  DeclContext *DC = D->getDeclContext();
  while (!DC->isModuleScopeContext()) {
    if (DC->isLocalContext())
      return FormalLinkage::Private;
    DC = DC->getParent();
  }

  // Clang declarations are public and can't be assured of having a
  // unique defining location.
  if (isa<ClangModuleUnit>(DC))
    return FormalLinkage::PublicNonUnique;

  // TODO: access control
  return FormalLinkage::PublicUnique;
}

FormalLinkage swift::getTypeLinkage(CanType type) {
  FormalLinkage result = FormalLinkage::Top;

  // Merge all nominal types from the structural type.
  (void) type.findIf([&](Type _type) {
    CanType type = CanType(_type);

    // For any nominal type reference, look at the type declaration.
    if (auto nominal = type->getAnyNominal()) {
      result ^= getDeclLinkage(nominal);

    // For polymorphic function types, look at the generic parameters.
    // FIXME: findIf should do this, once polymorphic function types can be
    // canonicalized and re-formed properly.
    } else if (auto polyFn = dyn_cast<PolymorphicFunctionType>(type)) {
      result ^= getGenericClauseLinkage(polyFn->getGenericParameters());
    }

    return false; // continue searching
  });

  return result;
}

/// Returns true if we are able to find an address projection path from V1 to
/// V2. Inserts the found path into Path.
bool
swift::
findAddressProjectionPathBetweenValues(SILValue V1, SILValue V2,
                                       SmallVectorImpl<Projection> &Path) {
  // If V1 == V2, there is a "trivial" address projection in between the
  // two. This is represented by returning true, but putting nothing into Path.
  if (V1 == V2)
    return true;

  // Otherwise see if V2 can be projection extracted from V1. First see if
  // V2 is a projection at all.
  auto Iter = V2;
  while (Projection::isAddressProjection(Iter) && V1 != Iter) {
    if (auto *SEA = dyn_cast<StructElementAddrInst>(Iter.getDef()))
      Path.push_back(Projection(SEA));
    else if (auto *TEA = dyn_cast<TupleElementAddrInst>(Iter.getDef()))
      Path.push_back(Projection(TEA));
    else
      Path.push_back(Projection(cast<RefElementAddrInst>(&*Iter)));
    Iter = cast<SILInstruction>(*Iter).getOperand(0);
  }

  // Return true if we have a non-empty projection list and if V1 == Iter.
  return !Path.empty() && V1 == Iter;
}
