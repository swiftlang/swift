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

/// Change the DeclContext of any contained parameters to the specified
/// DeclContext.
void ParameterList::setDeclContextOfParamDecls(DeclContext *DC) {
  for (ParamDecl *P : *this) {
    P->setDeclContext(DC);
    if (auto initContext = P->getCachedDefaultArgumentInitContext())
      (*initContext)->changeFunction(DC);
  }
}

/// Make a duplicate copy of this parameter list.  This allocates copies of
/// the ParamDecls, so they can be reparented into a new DeclContext.
ParameterList *ParameterList::clone(const ASTContext &C,
                                    OptionSet<CloneFlags> options) const {
  // TODO(distributed): copy types thanks to flag in options
  // If this list is empty, don't actually bother with a copy.
  if (size() == 0)
    return const_cast<ParameterList*>(this);
  
  SmallVector<ParamDecl*, 8> params(begin(), end());

  // Remap the ParamDecls inside of the ParameterList.
  unsigned i = 0;
  for (auto &decl : params) {
    auto defaultArgKind = decl->getDefaultArgumentKind();

    // If we're inheriting a default argument, mark it as such.
    // FIXME: Figure out how to clone default arguments as well.
    if (options & Inherited) {
      switch (defaultArgKind) {
      case DefaultArgumentKind::Normal:
      case DefaultArgumentKind::StoredProperty:
        defaultArgKind = DefaultArgumentKind::Inherited;
        break;

      default:
        break;
      }
    } else {
      defaultArgKind = DefaultArgumentKind::None;
    }

    decl = ParamDecl::cloneWithoutType(C, decl, defaultArgKind);
    if (options & Implicit)
      decl->setImplicit();

    // If the argument isn't named, give the parameter a name so that
    // silgen will produce a value for it.
    if (decl->getName().empty() && (options & NamedArguments)) {
      llvm::SmallString<16> s;
      { llvm::raw_svector_ostream(s) << "__argument" << ++i; }
      decl->setName(C.getIdentifier(s));
    }
  }
  
  return create(C, params);
}

void ParameterList::getParams(
                        SmallVectorImpl<AnyFunctionType::Param> &params) const {
  for (auto P : *this)
    params.push_back(P->toFunctionParam());
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

StringRef swift::getNameForParamSpecifier(ParamSpecifier specifier) {
  switch (specifier) {
  case ParamSpecifier::Default:
    return "default";
  case ParamSpecifier::InOut:
    return "inout";
  case ParamSpecifier::Borrowing:
    return "borrowing";
  case ParamSpecifier::Consuming:
    return "consuming";
  case ParamSpecifier::LegacyShared:
    return "__shared";
  case ParamSpecifier::LegacyOwned:
    return "__owned";
  case ParamSpecifier::ImplicitlyCopyableConsuming:
    return "implicitly_copyable_consuming";
  }
  llvm_unreachable("bad ParamSpecifier");
}

ParamSpecifier swift::getDefaultParamSpecifier(const ValueDecl *forDecl) {
  // `consuming` is the default ownership for initializers and setters.
  // Everything else defaults to borrowing.
  if (!forDecl) {
    return ParamSpecifier::Borrowing;
  }
  auto forFuncDecl = dyn_cast<AbstractFunctionDecl>(forDecl);
  if (!forFuncDecl) {
    return ParamSpecifier::Borrowing;
  }

  if (isa<ConstructorDecl>(forFuncDecl)) {
    return ParamSpecifier::Consuming;
  } else if (auto accessor = dyn_cast<AccessorDecl>(forFuncDecl)) {
    switch (accessor->getAccessorKind()) {
    case AccessorKind::Modify:
    case AccessorKind::Set:
      return ParamSpecifier::Consuming;
    default:
      return ParamSpecifier::Borrowing;
    }
  }

  return ParamSpecifier::Borrowing;
}

/// Returns true if an override with the given parameter ownership specifier is
/// allowed over a base method with a given parameter ownership specifier.
bool swift::paramSpecifiersMatch(const ValueDecl *baseDecl,
                                 ParamSpecifier baseSpec,
                                 ParamSpecifier derivedSpec) {
  // We allow for specifier mismatches between imported ObjC base methods and
  // Swift overrides.
  if (baseDecl && baseDecl->isObjC() && baseDecl->hasClangNode()) {
    return true;
  }

  // The default ownership for initializer parameters is consuming. For other
  // declarations it's borrowing.
  auto defaultSpecifier = isa_and_nonnull<ConstructorDecl>(baseDecl)
    ? ParamSpecifier::Consuming
    : ParamSpecifier::Borrowing;

  baseSpec = baseSpec == ParamSpecifier::Default
    ? defaultSpecifier : baseSpec;
  derivedSpec = derivedSpec == ParamSpecifier::Default
    ? defaultSpecifier : derivedSpec;

#define SPECS(b, d) (unsigned(b) << 16 | unsigned(d))
  switch ((SPECS(baseSpec, derivedSpec))) {
  // Exact matches are OK.
  case SPECS(ParamSpecifier::InOut, ParamSpecifier::InOut):
  case SPECS(ParamSpecifier::Borrowing, ParamSpecifier::Borrowing):
  case SPECS(ParamSpecifier::Consuming, ParamSpecifier::Consuming):
  case SPECS(ParamSpecifier::LegacyShared, ParamSpecifier::LegacyShared):
  case SPECS(ParamSpecifier::LegacyOwned, ParamSpecifier::LegacyOwned):
  case SPECS(ParamSpecifier::ImplicitlyCopyableConsuming,
             ParamSpecifier::ImplicitlyCopyableConsuming):

  // Modifiers from different eras may be matched as long as they have the same
  // external ownership behavior.
  case SPECS(ParamSpecifier::Borrowing, ParamSpecifier::LegacyShared):
  case SPECS(ParamSpecifier::LegacyShared, ParamSpecifier::Borrowing):
  case SPECS(ParamSpecifier::Consuming, ParamSpecifier::LegacyOwned):
  case SPECS(ParamSpecifier::Consuming,
             ParamSpecifier::ImplicitlyCopyableConsuming):
  case SPECS(ParamSpecifier::LegacyOwned, ParamSpecifier::Consuming):
  case SPECS(ParamSpecifier::LegacyOwned,
             ParamSpecifier::ImplicitlyCopyableConsuming):
  case SPECS(ParamSpecifier::ImplicitlyCopyableConsuming,
             ParamSpecifier::Consuming):
  case SPECS(ParamSpecifier::ImplicitlyCopyableConsuming,
             ParamSpecifier::LegacyOwned):

    return true;

  default:
    return false;
  }
#undef SPECS
}
