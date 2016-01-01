//===--- Parameter.cpp - Functions & closures parameters ------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the Parameter class, the ParameterList class and support
// logic.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Parameter.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Expr.h"
#include "swift/AST/ExprHandle.h"
using namespace swift;

/// Return the full source range of this parameter.
SourceRange Parameter::getSourceRange() const {
  SourceRange range;
  
  SourceLoc APINameLoc = decl->getArgumentNameLoc();
  SourceLoc nameLoc = decl->getNameLoc();
  
  if (APINameLoc.isValid() && nameLoc.isInvalid())
    range = APINameLoc;
  else if (APINameLoc.isInvalid() && nameLoc.isValid())
    range = nameLoc;
  else
    range = SourceRange(APINameLoc, nameLoc);

  if (range.isInvalid()) return range;
  
  // It would be nice to extend the front of the range to show where inout is,
  // but we don't have that location info.  Extend the back of the range to the
  // location of the default argument, or the typeloc if they are valid.
  if (auto expr = getDefaultValue()) {
    auto endLoc = expr->getExpr()->getEndLoc();
    if (endLoc.isValid())
      return SourceRange(range.Start, endLoc);
  }

  // If the typeloc has a valid location, use it to end the range.
  if (auto typeRepr = decl->getTypeLoc().getTypeRepr()) {
    auto endLoc = typeRepr->getEndLoc();
    if (endLoc.isValid())
      return SourceRange(range.Start, endLoc);
  }
  
  // Otherwise, just return the info we have about the parameter.
  return range;
}

Type Parameter::getVarargBaseTy(Type VarArgT) {
  TypeBase *T = VarArgT.getPointer();
  if (ArraySliceType *AT = dyn_cast<ArraySliceType>(T))
    return AT->getBaseType();
  if (BoundGenericType *BGT = dyn_cast<BoundGenericType>(T)) {
    // It's the stdlib Array<T>.
    return BGT->getGenericArgs()[0];
  }
  assert(isa<ErrorType>(T));
  return T;
}


/// TODO: unique and reuse the () parameter list in ASTContext, it is common to
/// many methods.  Other parameter lists cannot be uniqued because the decls
/// within them are always different anyway (they have different DeclContext's).
ParameterList *
ParameterList::create(const ASTContext &C, SourceLoc LParenLoc,
                      ArrayRef<Parameter> params, SourceLoc RParenLoc) {
  assert(LParenLoc.isValid() == RParenLoc.isValid() &&
         "Either both paren locs are valid or neither are");
  
  auto byteSize = sizeof(ParameterList)+params.size()*sizeof(Parameter);
  auto rawMem = C.Allocate(byteSize, alignof(ParameterList));
  
  //  Placement initialize the ParameterList and the Parameter's.
  auto PL = ::new (rawMem) ParameterList(LParenLoc, params.size(), RParenLoc);

  for (size_t i = 0, e = params.size(); i != e; ++i)
    ::new (&PL->get(i)) Parameter(params[i]);
  
  return PL;
}

/// Create an implicit 'self' decl for a method in the specified decl context.
/// If 'static' is true, then this is self for a static method in the type.
///
/// Note that this decl is created, but it is returned with an incorrect
/// DeclContext that needs to be set correctly.  This is automatically handled
/// when a function is created with this as part of its argument list.
///
ParameterList *ParameterList::createSelf(SourceLoc loc, DeclContext *DC,
                                         bool isStaticMethod, bool isInOut) {
  auto *PD = ParamDecl::createSelf(loc, DC, isStaticMethod, isInOut);
  return create(DC->getASTContext(), Parameter::withoutLoc(PD));
}

/// Change the DeclContext of any contained parameters to the specified
/// DeclContext.
void ParameterList::setDeclContextOfParamDecls(DeclContext *DC) {
  for (auto &P : *this)
    P.decl->setDeclContext(DC);
}



/// Make a duplicate copy of this parameter list.  This allocates copies of
/// the ParamDecls, so they can be reparented into a new DeclContext.
ParameterList *ParameterList::clone(const ASTContext &C,
                                    OptionSet<CloneFlags> options) const {
  // If this list is empty, don't actually bother with a copy.
  if (size() == 0)
    return const_cast<ParameterList*>(this);
  
  SmallVector<Parameter, 8> params(begin(), end());

  // Remap the ParamDecls inside of the ParameterList.
  for (auto &param : params) {
    auto decl = param.decl;
    auto name = decl->getName();
    // If the argument isn't named, and we're cloning for an inherited
    // constructor, give the parameter a name so that silgen will produce a
    // value for it.
    if (name.empty() && (options & Inherited))
      name = C.getIdentifier("argument");
    
    param.decl = new (C) ParamDecl(decl->isLet(),
                                   decl->getArgumentNameLoc(),
                                   decl->getArgumentName(),
                                   decl->getLoc(), name,
                                   decl->hasType() ? decl->getType() : Type(),
                                   decl->getDeclContext());
    if ((options & Implicit) || decl->isImplicit())
      param.decl->setImplicit();

    param.decl->getTypeLoc() = decl->getTypeLoc();
    
    // If we're inheriting a default argument, mark it as such.
    if (param.defaultArgumentKind != DefaultArgumentKind::None &&
        (options & Inherited)) {
      param.defaultArgumentKind = DefaultArgumentKind::Inherited;
      param.setDefaultValue(nullptr);
    }
  }
  
  return create(C, params);
}

/// Return a TupleType or ParenType for this parameter list.  This returns a
/// null type if one of the ParamDecls does not have a type set for it yet.
Type ParameterList::getType(const ASTContext &C) const {
  if (size() == 0)
    return TupleType::getEmpty(C);
  
  SmallVector<TupleTypeElt, 8> argumentInfo;
  
  for (auto &P : *this) {
    if (!P.decl->hasType()) return Type();
    
    argumentInfo.push_back({
      P.decl->getType(), P.decl->getArgumentName(), P.defaultArgumentKind,
      P.isVariadic()
    });
  }
  
  return TupleType::get(argumentInfo, C);
}


/// Return the full function type for a set of curried parameter lists that
/// returns the specified result type.  This returns a null type if one of the
/// ParamDecls does not have a type set for it yet.
///
Type ParameterList::getFullType(Type resultType, ArrayRef<ParameterList*> PLL) {
  auto result = resultType;
  auto &C = result->getASTContext();
  
  for (auto PL : reversed(PLL)) {
    auto paramType = PL->getType(C);
    if (!paramType) return Type();
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
    auto Start = get(0).getStartLoc();
    auto End = getArray().back().getEndLoc();
    if (Start.isValid() && End.isValid())
      return { Start, End };
  }

  return SourceRange();
}

