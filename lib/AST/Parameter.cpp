//===--- Parameter.cpp - Functions & closures parameters ------------------===//
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
//
// This file defines the Parameter class, the ParameterList class and support
// logic.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ParameterList.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Types.h"
using namespace swift;

/// TODO: unique and reuse the () parameter list in ASTContext, it is common to
/// many methods.  Other parameter lists cannot be uniqued because the decls
/// within them are always different anyway (they have different DeclContext's).
ParameterList *
ParameterList::create(const ASTContext &C, SourceLoc LParenLoc,
                      ArrayRef<ParamDecl*> params, SourceLoc RParenLoc) {
  assert(LParenLoc.isValid() == RParenLoc.isValid() &&
         "Either both paren locs are valid or neither are");
  
  auto byteSize = totalSizeToAlloc<ParamDecl *>(params.size());
  auto rawMem = C.Allocate(byteSize, alignof(ParameterList));
  
  //  Placement initialize the ParameterList and the Parameter's.
  auto PL = ::new (rawMem) ParameterList(LParenLoc, params.size(), RParenLoc);

  std::uninitialized_copy(params.begin(), params.end(), PL->getArray().begin());

  return PL;
}

/// Create an implicit 'self' decl for a method in the specified decl context.
/// If 'static' is true, then this is self for a static method in the type.
///
/// Note that this decl is created, but it is returned with an incorrect
/// DeclContext that needs to be set correctly.  This is automatically handled
/// when a function is created with this as part of its argument list.
/// For a generic context, this also gives the parameter an unbound generic
/// type with the expectation that type-checking will fill in the context
/// generic parameters.
ParameterList *ParameterList::createUnboundSelf(SourceLoc loc,
                                                DeclContext *DC) {
  auto *PD = ParamDecl::createUnboundSelf(loc, DC);
  return create(DC->getASTContext(), PD);
}

/// Create an implicit 'self' decl for a method in the specified decl context.
/// If 'static' is true, then this is self for a static method in the type.
///
/// Note that this decl is created, but it is returned with an incorrect
/// DeclContext that needs to be set correctly.  This is automatically handled
/// when a function is created with this as part of its argument list.
ParameterList *ParameterList::createSelf(SourceLoc loc,
                                         DeclContext *DC,
                                         bool isStaticMethod,
                                         bool isInOut) {
  auto *PD = ParamDecl::createSelf(loc, DC, isStaticMethod, isInOut);
  return create(DC->getASTContext(), PD);
}

/// Change the DeclContext of any contained parameters to the specified
/// DeclContext.
void ParameterList::setDeclContextOfParamDecls(DeclContext *DC) {
  for (auto P : *this)
    P->setDeclContext(DC);
}

/// Make a duplicate copy of this parameter list.  This allocates copies of
/// the ParamDecls, so they can be reparented into a new DeclContext.
ParameterList *ParameterList::clone(const ASTContext &C,
                                    OptionSet<CloneFlags> options) const {
  // If this list is empty, don't actually bother with a copy.
  if (size() == 0)
    return const_cast<ParameterList*>(this);
  
  SmallVector<ParamDecl*, 8> params(begin(), end());

  // Remap the ParamDecls inside of the ParameterList.
  for (auto &decl : params) {
    bool hadDefaultArgument =
        decl->getDefaultArgumentKind() == DefaultArgumentKind::Normal;

    decl = new (C) ParamDecl(decl);
    if (options & Implicit)
      decl->setImplicit();

    // If the argument isn't named, and we're cloning for an inherited
    // constructor, give the parameter a name so that silgen will produce a
    // value for it.
    if (decl->getName().empty() && (options & Inherited))
      decl->setName(C.getIdentifier("argument"));
    
    // If we're inheriting a default argument, mark it as such.
    // FIXME: Figure out how to clone default arguments as well.
    if (hadDefaultArgument) {
      if (options & Inherited)
        decl->setDefaultArgumentKind(DefaultArgumentKind::Inherited);
      else
        decl->setDefaultArgumentKind(DefaultArgumentKind::None);
    }
  }
  
  return create(C, params);
}

/// Return a TupleType or ParenType for this parameter list, written in terms
/// of contextual archetypes.
Type ParameterList::getType(const ASTContext &C) const {
  if (size() == 0)
    return TupleType::getEmpty(C);
  
  SmallVector<TupleTypeElt, 8> argumentInfo;
  
  for (auto P : *this) {
    argumentInfo.emplace_back(
        P->getType(), P->getArgumentName(),
        ParameterTypeFlags::fromParameterType(P->getType(), P->isVariadic()));
  }

  return TupleType::get(argumentInfo, C);
}

/// Return a TupleType or ParenType for this parameter list, written in terms
/// of interface types.
Type ParameterList::getInterfaceType(const ASTContext &C) const {
  if (size() == 0)
    return TupleType::getEmpty(C);

  SmallVector<TupleTypeElt, 8> argumentInfo;

  for (auto P : *this) {
    auto type = P->getInterfaceType();
    assert(!type->hasArchetype());

    argumentInfo.emplace_back(
        type, P->getArgumentName(),
        ParameterTypeFlags::fromParameterType(type, P->isVariadic()));
  }

  return TupleType::get(argumentInfo, C);
}


/// Return the full function type for a set of curried parameter lists that
/// returns the specified result type.  This returns a null type if one of the
/// ParamDecls does not have a type set for it yet.
///
Type ParameterList::getFullInterfaceType(Type resultType,
                                         ArrayRef<ParameterList*> PLL,
                                         const ASTContext &C) {
  auto result = resultType;
  for (auto PL : reversed(PLL)) {
    auto paramType = PL->getInterfaceType(C);
    result = FunctionType::get(paramType, result);
  }
  return result;
}


/// Return the full source range of this parameter list.
SourceRange ParameterList::getSourceRange() const {
  // If we have locations for the parens, then they define our range.
  if (LParenLoc.isValid())
    return { LParenLoc, RParenLoc };
  
  // Otherwise, try the first and last parameter.
  if (size() != 0) {
    auto Start = get(0)->getStartLoc();
    auto End = getArray().back()->getEndLoc();
    if (Start.isValid() && End.isValid())
      return { Start, End };
  }

  return SourceRange();
}

