//===--- ASTGenType.cpp ---------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/GenericParamList.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Parse/ASTGen.h"
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "swift/Parse/Parser.h"

using namespace swift;
using namespace swift::syntax;

TypeRepr *ASTGen::generate(const syntax::TypeSyntax &Type,
                           const SourceLoc Loc) {
  auto typeLoc = advanceLocBegin(Loc, Type);

  // Check if we have recorded a type that hasn't been migrated to
  // libSyntax-parsing yet at this location
  if (hasType(typeLoc)) {
    return takeType(typeLoc);
  }

  // Otherwise, generate the AST node for the type.
  if (auto array = Type.getAs<ArrayTypeSyntax>()) {
    return generate(*array, Loc);
  } else if (auto Attributed = Type.getAs<AttributedTypeSyntax>()) {
    return generate(*Attributed, Loc);
  } else if (auto ClassRestriction = Type.getAs<ClassRestrictionTypeSyntax>()) {
    return generate(*ClassRestriction, Loc);
  } else if (auto completionTy = Type.getAs<CodeCompletionTypeSyntax>()) {
    return generate(*completionTy, Loc);
  } else if (auto CompositionList =
                 Type.getAs<CompositionTypeElementListSyntax>()) {
    llvm_unreachable("Composition elements list is being generated from "
                     "within the CompositionType generate function.");
  } else if (auto CompositionElement =
                 Type.getAs<CompositionTypeElementSyntax>()) {
    llvm_unreachable("Composition type elements are being generated from "
                     "within the CompositionType generate function.");
  } else if (auto Composition = Type.getAs<CompositionTypeSyntax>()) {
    return generate(*Composition, Loc);
  } else if (auto dictionary = Type.getAs<DictionaryTypeSyntax>()) {
    return generate(*dictionary, Loc);
  } else if (auto Function = Type.getAs<FunctionTypeSyntax>()) {
    return generate(*Function, Loc);
  } else if (auto Unwrapped =
                 Type.getAs<ImplicitlyUnwrappedOptionalTypeSyntax>()) {
    return generate(*Unwrapped, Loc);
  } else if (auto memberIdentifier = Type.getAs<MemberTypeIdentifierSyntax>()) {
    return generate(*memberIdentifier, Loc);
  } else if (auto Metatype = Type.getAs<MetatypeTypeSyntax>()) {
    return generate(*Metatype, Loc);
  } else if (auto Optional = Type.getAs<OptionalTypeSyntax>()) {
    return generate(*Optional, Loc);
  } else if (auto SILBoxType = Type.getAs<SILBoxTypeSyntax>()) {
    return generate(*SILBoxType, Loc);
  } else if (auto SILFunctionType = Type.getAs<SILFunctionTypeSyntax>()) {
    return generate(*SILFunctionType, Loc);
  } else if (auto simpleIdentifier = Type.getAs<SimpleTypeIdentifierSyntax>()) {
    return generate(*simpleIdentifier, Loc);
  } else if (auto Some = Type.getAs<SomeTypeSyntax>()) {
    return generate(*Some, Loc);
  } else if (auto Tuple = Type.getAs<TupleTypeSyntax>()) {
    return generate(*Tuple, Loc);
  } else if (auto Tuple = Type.getAs<TupleTypeElementSyntax>()) {
    llvm_unreachable("Tuple type elements are being generated from within the "
                     "TupleTypeSyntax generate function.");
  } else if (auto unknown = Type.getAs<UnknownTypeSyntax>()) {
    return generate(*unknown, Loc);
  } else {
    llvm_unreachable("ASTGen hasn't been tought how to generate this type");
  }

// FIXME: (syntax-parse) Erase generic type parameters
//  if (Tok.is(tok::arrow)) {
//  } else if (auto firstGenerics = generics ? generics : patternGenerics) {
//    // Only function types may be generic.
//    auto brackets = firstGenerics->getSourceRange();
//    diagnose(brackets.Start, diag::generic_non_function);
//
//    // Forget any generic parameters we saw in the type.
//    class EraseTypeParamWalker : public ASTWalker {
//    public:
//      bool walkToTypeReprPre(TypeRepr *T) override {
//        if (auto ident = dyn_cast<ComponentIdentTypeRepr>(T)) {
//          if (auto decl = ident->getBoundDecl()) {
//            if (auto genericParam = dyn_cast<GenericTypeParamDecl>(decl))
//              ident->overwriteNameRef(genericParam->createNameRef());
//          }
//        }
//        return true;
//      }
//
//    } walker;
//
//    if (tyR)
//      tyR->walk(walker);
//  }
}

TypeRepr *ASTGen::generate(const syntax::ArrayTypeSyntax &Type,
                           const SourceLoc Loc) {
  SourceLoc LBracketLoc = advanceLocBegin(Loc, Type);
  SourceLoc RBracketLoc = advanceLocEnd(Loc, Type);

  TypeRepr *ElementType = generate(Type.getElementType(), Loc);
  if (!ElementType) {
    return nullptr;
  }
  return new (Context) ArrayTypeRepr(ElementType, {LBracketLoc, RBracketLoc});
}

TypeRepr *ASTGen::generate(const AttributedTypeSyntax &Type,
                           const SourceLoc Loc) {
  auto typeAST = generate(Type.getBaseType(), Loc);
  if (!typeAST) {
    return nullptr;
  }

  if (auto attributes = Type.getAttributes()) {
    TypeAttributes attrs = generateTypeAttributes(*attributes, Loc);
    if (!attrs.empty()) {
      typeAST = new (Context) AttributedTypeRepr(attrs, typeAST);
    }
  }

  if (auto specifier = Type.getSpecifier()) {
    auto specifierLoc = advanceLocBegin(Loc, *specifier);
    auto specifierText = specifier->getText();

    // don't apply multiple specifiers to a type: that's invalid and was already
    // reported in the parser, handle gracefully
    if (!isa<SpecifierTypeRepr>(typeAST)) {
      if (specifierText == "inout") {
        typeAST = new (Context) InOutTypeRepr(typeAST, specifierLoc);
      } else if (specifierText == "__owned") {
        typeAST = new (Context) OwnedTypeRepr(typeAST, specifierLoc);
      } else if (specifierText == "__shared") {
        typeAST = new (Context) SharedTypeRepr(typeAST, specifierLoc);
      }
    }
  }

  return typeAST;
}

TypeRepr *ASTGen::generate(const syntax::ClassRestrictionTypeSyntax &Type,
                           const SourceLoc Loc) {
  auto declNameLoc = DeclNameLoc(advanceLocBegin(Loc, Type.getClassKeyword()));
  auto declNameRef = DeclNameRef(Context.getIdentifier("AnyObject"));
  return new (Context) SimpleIdentTypeRepr(declNameLoc, declNameRef);
}

TypeRepr *ASTGen::generate(const syntax::CodeCompletionTypeSyntax &Type,
                           const SourceLoc Loc) {
  auto base = Type.getBase();
  if (!base) {
    if (P.CodeCompletion) {
      P.CodeCompletion->completeTypeSimpleBeginning();
    }
    return nullptr;
  }

  if (auto *parsedTyR = generate(*base, Loc)) {
    if (P.CodeCompletion) {
      P.CodeCompletion->setParsedTypeLoc(parsedTyR);
      if (Type.getPeriod()) {
        P.CodeCompletion->completeTypeIdentifierWithDot();
      } else {
        P.CodeCompletion->completeTypeIdentifierWithoutDot();
      }
    }
    return parsedTyR;
  }

  return nullptr;
}

TypeRepr *ASTGen::generate(const syntax::CompositionTypeSyntax &Type,
                           const SourceLoc Loc) {
  auto elements = Type.getElements();
  assert(!elements.empty());

  SmallVector<TypeRepr *, 4> elementTypeReprs;
  for (auto element = elements.begin(); element != elements.end(); ++element) {
    auto elementType = (*element).getType();
    TypeRepr *elementTypeRepr;
    if (auto someSyntax = elementType.getAs<SomeTypeSyntax>()) {
      // the invalid `some` after an ampersand was already diagnosed by the
      // parser, handle it gracefully
      elementTypeRepr = generate(someSyntax->getBaseType(), Loc);
    } else {
      elementTypeRepr = generate(elementType, Loc);
    }

    if (elementTypeRepr) {
      elementTypeReprs.push_back(elementTypeRepr);
    }
  }

  auto firstTypeLoc = advanceLocBegin(Loc, elements[0]);
  auto lastTypeLoc = advanceLocBegin(Loc, elements[elements.size() - 1]);
  return CompositionTypeRepr::create(Context, elementTypeReprs, firstTypeLoc,
                                     {firstTypeLoc, lastTypeLoc});
}

TypeRepr *ASTGen::generate(const syntax::DictionaryTypeSyntax &Type,
                           const SourceLoc Loc) {
  SourceLoc LBracketLoc = advanceLocBegin(Loc, Type);
  SourceLoc ColonLoc = advanceLocBegin(Loc, Type.getColon());
  SourceLoc RBracketLoc = advanceLocEnd(Loc, Type);

  TypeRepr *KeyType = generate(Type.getKeyType(), Loc);
  TypeRepr *ValueType = generate(Type.getValueType(), Loc);
  if (!KeyType || !ValueType) {
    return nullptr;
  }

  SourceRange Range{LBracketLoc, RBracketLoc};
  return new (Context) DictionaryTypeRepr(KeyType, ValueType, ColonLoc, Range);
}

TypeRepr *ASTGen::generate(const syntax::FunctionTypeSyntax &FuncSyntax,
                           const SourceLoc Loc) {
  TupleTypeRepr *argumentTypes = argumentTypes = generateTuple(FuncSyntax.getLeftParen(), FuncSyntax.getArguments(), FuncSyntax.getRightParen(), Loc);

  if (!argumentTypes) {
    return nullptr;
  }

  SourceLoc asyncLoc;
  if (FuncSyntax.getAsyncKeyword()) {
    asyncLoc = advanceLocBegin(Loc, *FuncSyntax.getAsyncKeyword());
  }
  SourceLoc throwsLoc;
  if (FuncSyntax.getThrowsOrRethrowsKeyword()) {
    throwsLoc = advanceLocBegin(Loc, *FuncSyntax.getThrowsOrRethrowsKeyword());
  }

  auto arrowLoc = advanceLocBegin(Loc, FuncSyntax.getArrow());
  auto returnType = generate(FuncSyntax.getReturnType(), Loc);
  if (!returnType) {
    return nullptr;
  }

  return new (Context) FunctionTypeRepr(nullptr, argumentTypes, asyncLoc,
                                        throwsLoc, arrowLoc, returnType);
}

TypeRepr *
ASTGen::generate(const syntax::ImplicitlyUnwrappedOptionalTypeSyntax &Type,
                 const SourceLoc Loc) {
  auto baseTypeRepr = generate(Type.getWrappedType(), Loc);
  auto exclamationLoc = advanceLocBegin(Loc, Type.getExclamationMark());
  return new (Context)
      ImplicitlyUnwrappedOptionalTypeRepr(baseTypeRepr, exclamationLoc);
}

TypeRepr *ASTGen::generate(const syntax::MemberTypeIdentifierSyntax &Type,
                           const SourceLoc Loc) {
  SmallVector<ComponentIdentTypeRepr *, 4> components;
  gatherTypeIdentifierComponents(Type, Loc, components);
  return IdentTypeRepr::create(Context, components);
}

TypeRepr *ASTGen::generate(const syntax::MetatypeTypeSyntax &Type,
                           const SourceLoc Loc) {
  auto baseTypeRepr = generate(Type.getBaseType(), Loc);
  auto metaLoc = advanceLocBegin(Loc, Type.getTypeOrProtocol());
  auto metaText = Type.getTypeOrProtocol().getText();
  if (metaText == "Type") {
    return new (Context) MetatypeTypeRepr(baseTypeRepr, metaLoc);
  } else if (metaText == "Protocol") {
    return new (Context) ProtocolTypeRepr(baseTypeRepr, metaLoc);
  } else {
    llvm_unreachable("Meta part must be 'Type' or 'Protocol'");
  }
}

TypeRepr *ASTGen::generate(const syntax::OptionalTypeSyntax &Type,
                           const SourceLoc Loc) {
  auto baseTypeRepr = generate(Type.getWrappedType(), Loc);
  auto questionLoc = advanceLocBegin(Loc, Type.getQuestionMark());
  return new (Context) OptionalTypeRepr(baseTypeRepr, questionLoc);
}

TypeRepr *ASTGen::generate(const syntax::SILBoxTypeSyntax &Type, const SourceLoc Loc) {
  GenericParamList *generics = nullptr;
  if (auto genericParams = Type.getGenericParameterClauses()) {
    generics = generate(*genericParams, Loc);
  }
  auto leftBraceLoc = advanceLocBegin(Loc, Type.getLeftBrace());

  SmallVector<SILBoxTypeRepr::Field, 4> fields;
  for (auto field : Type.getFields()) {
    SourceLoc varOrLetLoc = advanceLocBegin(Loc, field.getSpecifier());
    bool isMutable;
    switch (field.getSpecifier().getTokenKind()) {
    case tok::kw_var: isMutable = true; break;
    case tok::kw_let: isMutable = false; break;
    default: llvm_unreachable("Specifier must be 'let' or 'var'"); break;
    }
    auto fieldType = generate(field.getType(), Loc);
    fields.push_back({varOrLetLoc, isMutable, fieldType});
  }
  auto rightBraceLoc = advanceLocBegin(Loc, Type.getRightBrace());

  SourceLoc genericLeftAngleLoc;
  SmallVector<TypeRepr *, 4> genericArgs;
  SourceLoc genericRightAngleLoc;

  if (auto genericArgClause = Type.getGenericArgumentClause()) {
    generateGenericArgs(*genericArgClause, Loc, genericLeftAngleLoc, genericRightAngleLoc, genericArgs);
  }

  return SILBoxTypeRepr::create(Context, generics, leftBraceLoc, fields, rightBraceLoc, genericLeftAngleLoc, Context.AllocateCopy(genericArgs), genericRightAngleLoc);
}

TypeRepr *ASTGen::generate(const syntax::SILFunctionTypeSyntax &Type,
                   const SourceLoc Loc) {
  GenericParamList *generics = nullptr;
  if (auto genericParamsSyntax = Type.getGenericParameterClauses()) {
    generics = generate(*genericParamsSyntax, Loc);
  }

  GenericParamList *patternGenerics = nullptr;
  if (auto patternGenericSyntax = Type.getPatternGenericParameterClauses()) {
    patternGenerics = generate(*patternGenericSyntax, Loc);
  }

  auto funcType = cast<FunctionTypeRepr>(generate(Type.getFunction(), Loc));

  SmallVector<TypeRepr *, 2> invocationSubstitutions;
  if (auto genericSubsSyntax = Type.getGenericSubstitution()) {
    invocationSubstitutions = generate(*genericSubsSyntax, Loc);
  }

  SmallVector<TypeRepr *, 2> patternSubstitutions;
  if (auto patternSubsSyntax = Type.getGenericPatternSubstitution()) {
    patternSubstitutions = generate(*patternSubsSyntax, Loc);
  }

  if (Type.getSubstitutedAttrAtToken() && patternSubstitutions.empty()) {
    auto diagLoc = advanceLocEnd(Loc, Type);
    P.diagnose(diagLoc, diag::sil_function_subst_expected_subs);
  }

  return new (Context)
      FunctionTypeRepr(generics, funcType->getArgsTypeRepr(), funcType->getAsyncLoc(), funcType->getThrowsLoc(), funcType->getArrowLoc(), funcType->getResultTypeRepr(), patternGenerics, Context.AllocateCopy(patternSubstitutions), Context.AllocateCopy(invocationSubstitutions));
}

DeclAttributes
ASTGen::generateDeclAttributes(const Syntax &D, SourceLoc Loc,
                               bool includeComments) {
  // Find the AST attribute-list from the lookup table.
  if (auto firstTok = D.getFirstToken()) {
    auto declLoc = advanceLocBegin(Loc, *firstTok);
    if (hasDeclAttributes(declLoc))
      return takeDeclAttributes(declLoc);
  }
  return DeclAttributes();
}

////////////////////////////////////////////////////////////////////////////////////////////////////////// Move to ASTGenGeneric

SmallVector<TypeRepr *, 2>
ASTGen::generate(const GenericSubstitutionSyntax &substitution, const SourceLoc Loc) {
  SmallVector<TypeRepr *, 2> substitutionTypes;
  substitutionTypes.reserve(substitution.getSubstitutions().size());
  for (auto subst : substitution.getSubstitutions()) {
    auto typeRepr = generate(subst.getType(), Loc);
    substitutionTypes.push_back(typeRepr);
  }
  return substitutionTypes;
}

GenericParamList *
ASTGen::generate(const GenericParameterClauseListSyntax &clauses,
                 const SourceLoc Loc) {
  GenericParamList *curr = nullptr;

  // The first one is the outmost generic parameter list.
  for (const auto &clause : clauses) {
    auto params = generate(clause, Loc);
    if (!params) {
      continue;
    }
    if (curr) {
      params->setOuterParameters(curr);
    }
    curr = params;
  }

  return curr;
}

GenericParamList *ASTGen::generate(const syntax::GenericParameterClauseSyntax &clause, const SourceLoc Loc) {
  SmallVector<GenericTypeParamDecl *, 4> params;
  params.reserve(clause.getGenericParameterList().getNumChildren());

  for (auto elem : clause.getGenericParameterList()) {
    auto nameTok = elem.getName();
    if (nameTok.isMissing())
      break;

    DeclAttributes attrs = generateDeclAttributes(elem, Loc, false);
    Identifier name = Context.getIdentifier(elem.getName().getIdentifierText());
    SourceLoc nameLoc = advanceLocBegin(Loc, elem.getName());

    // We always create generic type parameters with an invalid depth.
    // Semantic analysis fills in the depth when it processes the generic
    // parameter list.
    // FIXME: (syntax-parse) We shouldn't be accessing the current decl context
    // of the parser. It might have moved to a different declaration before
    // ASTGen is being invoked.
    auto param = new (Context)
        GenericTypeParamDecl(P.CurDeclContext, name, nameLoc,
                             GenericTypeParamDecl::InvalidDepth, params.size());

    if (auto inherited = elem.getInheritedType()) {
      if (auto ty = generate(*inherited, Loc)) {
        SmallVector<TypeLoc, 1> constraints = {ty};
        param->setInherited(Context.AllocateCopy(constraints));
      }
    }

    // Attach attributes.
    param->getAttrs() = attrs;

    params.push_back(param);
  }
  if (params.empty())
    return nullptr;

  SourceLoc whereLoc;
  SmallVector<RequirementRepr, 4> requirements;
  if (auto whereClause = clause.getObsoletedWhereClause()) {
    requirements.reserve(whereClause->getRequirementList().size());
    for (auto elem : whereClause->getRequirementList()) {
      if (auto req = generate(elem, Loc)) {
        requirements.push_back(*req);
      }
    }
    // There's an invariant that valid 'where' loc means that there's at
    // at least one valid requirement.
    if (!requirements.empty()) {
      whereLoc = advanceLocBegin(Loc, whereClause->getWhereKeyword());
    }
  }

  auto lAngleLoc = advanceLocBegin(Loc, clause);
  auto rAngleLoc = advanceLocEnd(Loc, clause);
  return GenericParamList::create(Context, lAngleLoc, params, whereLoc,
                                  requirements, rAngleLoc);
}

Optional<RequirementRepr>
ASTGen::generate(const syntax::GenericRequirementSyntax &req,
                 const SourceLoc Loc) {
  if (auto sameTypeReq = req.getBody().getAs<SameTypeRequirementSyntax>()) {
    auto firstType = generate(sameTypeReq->getLeftTypeIdentifier(), Loc);
    auto equalLoc = advanceLocBegin(Loc, sameTypeReq->getEqualityToken());
    auto secondType = generate(sameTypeReq->getRightTypeIdentifier(), Loc);
    if (!firstType || !secondType) {
      return None;
    }
    return RequirementRepr::getSameType(firstType, equalLoc, secondType);
  } else if (auto conformanceReq =
                 req.getBody().getAs<ConformanceRequirementSyntax>()) {
    auto firstType = generate(conformanceReq->getLeftTypeIdentifier(), Loc);
    auto secondType = generate(conformanceReq->getRightTypeIdentifier(), Loc);
    if (!firstType || !secondType) {
      return None;
    }
    return RequirementRepr::getTypeConstraint(
        firstType, advanceLocBegin(Loc, conformanceReq->getColon()),
        secondType);
  } else if (auto layoutReq = req.getBody().getAs<LayoutRequirementSyntax>()) {
    auto firstType = generate(layoutReq->getLeftTypeIdentifier(), Loc);
    auto layout = generate(layoutReq->getLayoutConstraint(), Loc);
    if (!firstType || layout.isNull()) {
      return None;
    }
    auto colonLoc = advanceLocBegin(Loc, layoutReq->getColon());
    auto layoutLoc = advanceLocBegin(Loc, layoutReq->getLayoutConstraint());
    return RequirementRepr::getLayoutConstraint(
        firstType, colonLoc, LayoutConstraintLoc(layout, layoutLoc));
  } else {
    llvm_unreachable("invalid syntax kind for requirement body");
  }
}

static LayoutConstraintKind getLayoutConstraintKind(Identifier &id,
                                                    ASTContext &Ctx) {
  if (id == Ctx.Id_TrivialLayout)
    return LayoutConstraintKind::TrivialOfExactSize;
  if (id == Ctx.Id_TrivialAtMostLayout)
    return LayoutConstraintKind::TrivialOfAtMostSize;
  if (id == Ctx.Id_RefCountedObjectLayout)
    return LayoutConstraintKind::RefCountedObject;
  if (id == Ctx.Id_NativeRefCountedObjectLayout)
    return LayoutConstraintKind::NativeRefCountedObject;
  if (id == Ctx.Id_ClassLayout)
    return LayoutConstraintKind::Class;
  if (id == Ctx.Id_NativeClassLayout)
    return LayoutConstraintKind::NativeClass;
  return LayoutConstraintKind::UnknownLayout;
}

LayoutConstraint ASTGen::generate(const LayoutConstraintSyntax &constraint,
                                  const SourceLoc Loc) {
  auto name = Context.getIdentifier(constraint.getName().getIdentifierText());
  auto constraintKind = getLayoutConstraintKind(name, Context);
  assert(constraintKind != LayoutConstraintKind::UnknownLayout);

  // Non-trivial constraint kinds don't have size/alignment.
  // TODO: Diagnose if it's supplied?
  if (!LayoutConstraintInfo::isTrivial(constraintKind)) {
    return LayoutConstraint::getLayoutConstraint(constraintKind, Context);
  }

  // '_Trivial' without explicit size/alignment.
  if (!constraint.getSize()) {
    return LayoutConstraint::getLayoutConstraint(LayoutConstraintKind::Trivial,
                                                 Context);
  }

  int size = 0;
  if (auto sizeSyntax = constraint.getSize()) {
    sizeSyntax->getText().getAsInteger(10, size);
  }
  if (size < 0) {
    return LayoutConstraint::getUnknownLayout();
  }

  int alignment = 0;
  if (auto alignmentSyntax = constraint.getAlignment()) {
    alignmentSyntax->getText().getAsInteger(10, alignment);
  }
  if (alignment > 0) {
    return LayoutConstraint::getUnknownLayout();
  }

  return LayoutConstraint::getLayoutConstraint(constraintKind, size, alignment,
                                               Context);
}

TypeRepr *ASTGen::generate(const SimpleTypeIdentifierSyntax &Type,
                           const SourceLoc Loc) {
  if (Type.getName().getTokenKind() == tok::kw_Any) {
    auto anyLoc = advanceLocBegin(Loc, Type.getName());
    return CompositionTypeRepr::createEmptyComposition(Context, anyLoc);
  }

  auto typeRepr = generateTypeIdentifier(Type, Loc);
  return IdentTypeRepr::create(Context, {typeRepr});
}

TypeRepr *ASTGen::generate(const syntax::SomeTypeSyntax &Type,
                           const SourceLoc Loc) {
  auto someLoc = advanceLocBegin(Loc, Type.getSomeSpecifier());
  auto baseTypeRepr = generate(Type.getBaseType(), Loc);
  return new (Context) OpaqueReturnTypeRepr(someLoc, baseTypeRepr);
}

TypeRepr *ASTGen::generate(const TupleTypeSyntax &Type, const SourceLoc Loc) {
  return generateTuple(Type.getLeftParen(), Type.getElements(),
                       Type.getRightParen(), Loc);
}

TypeRepr *ASTGen::generate(const syntax::UnknownTypeSyntax &Type,
                           const SourceLoc Loc) {
  auto ChildrenCount = Type.getNumChildren();

  if (auto recovered = recoverOldStyleProtocolComposition(Type, Loc)) {
    return recovered;
  }

  // Recovery failed.
  // Generate child 'TypeSyntax' anyway to trigger the side effects e.g.
  // code-completion.
  for (size_t i = 0; i != ChildrenCount; ++i) {
    auto elem = *Type.getChild(i);
    if (auto ty = elem.getAs<TypeSyntax>())
      (void)generate(*ty, Loc);
  }

  return nullptr;
}

void ASTGen::addType(TypeRepr *E, const SourceLoc Loc) {
  assert(!hasType(Loc));
  Types[Loc] = E;
}

bool ASTGen::hasType(const SourceLoc Loc) const {
  return Types.find(Loc) != Types.end();
}

TypeRepr *ASTGen::takeType(const SourceLoc Loc) {
  auto I = Types.find(Loc);
  assert(I != Types.end());
  auto expr = I->second;
  Types.erase(I);
  return expr;
}
  
//===--------------------------------------------------------------------===//
// MARK: - Private

void ASTGen::generateGenericArgs(
    const GenericArgumentClauseSyntax &ClauseSyntax, const SourceLoc Loc,
    SourceLoc &LAngleLoc, SourceLoc &RAngleLoc,
    SmallVectorImpl<TypeRepr *> &Args) {
  LAngleLoc = advanceLocBegin(Loc, ClauseSyntax);
  RAngleLoc = advanceLocEnd(Loc, ClauseSyntax);

  assert(Args.empty());
  for (auto arg : ClauseSyntax.getArguments()) {
    auto typeRepr = generate(arg.getArgumentType(), Loc);
    if (!typeRepr) {
      typeRepr = new (Context) ErrorTypeRepr(advanceLocBegin(Loc, arg));
    }
    Args.push_back(typeRepr);
  }
}

TupleTypeRepr *ASTGen::generateTuple(const TokenSyntax &LParen,
                                     const TupleTypeElementListSyntax &Elements,
                                     const TokenSyntax &RParen,
                                     const SourceLoc Loc) {
  auto leftParenLoc = advanceLocBegin(Loc, LParen);
  auto rightParenLoc = advanceLocEnd(Loc, RParen);

  SmallVector<TupleTypeReprElement, 4> tupleElements;

  SourceLoc ellipsisLoc;
  unsigned ellipsisIdx;

  for (unsigned i = 0; i < Elements.size(); i++) {
    auto element = Elements[i];
    TupleTypeReprElement elementAST;
    elementAST.Type = generate(element.getType(), Loc);
    if (!elementAST.Type) {
      // If the type cannot be parsed, we cannot form a meaningful tuple element
      // Continue and don't add it to Elements.
      continue;
    }

    if (auto name = element.getName()) {
      elementAST.NameLoc = advanceLocBegin(Loc, *name);
      elementAST.Name = name->getText() == "_"
                            ? Identifier()
                            : Context.getIdentifier(name->getIdentifierText());
    }
    if (auto colon = element.getColon()) {
      elementAST.ColonLoc = advanceLocBegin(Loc, *colon);
    }
    if (auto secondName = element.getSecondName()) {
      elementAST.SecondNameLoc = advanceLocBegin(Loc, *secondName);
      elementAST.SecondName =
          secondName->getText() == "_"
              ? Identifier()
              : Context.getIdentifier(secondName->getIdentifierText());
      if (elementAST.Name.empty()) {
        // If the first name is empty (i.e. was an underscore), use it as the
        // underscore location and use the second (non-underscore) name as the
        // first name.
        elementAST.UnderscoreLoc = elementAST.NameLoc;
        elementAST.Name = elementAST.SecondName;
        elementAST.NameLoc = elementAST.SecondNameLoc;
      }
    }

    if (auto inOut = element.getInOut()) {
      // don't apply multiple inout specifiers to a type: that's invalid and was
      // already reported in the parser, handle gracefully
      if (!isa<InOutTypeRepr>(elementAST.Type)) {
        auto inOutLoc = advanceLocBegin(Loc, *inOut);
        elementAST.Type =
            new (Context) InOutTypeRepr(elementAST.Type, inOutLoc);
      }
    }
    if (auto comma = element.getTrailingComma()) {
      elementAST.TrailingCommaLoc = advanceLocBegin(Loc, *comma);
    }

    if (auto ellipsis = element.getEllipsis()) {
      // If we have multiple ellipsis, they have already been diagnosed in the
      // parser. Just consider the first one.
      if (ellipsisLoc.isInvalid()) {
        ellipsisLoc = advanceLocBegin(Loc, *ellipsis);
        ellipsisIdx = i;
      }
    }
    tupleElements.push_back(elementAST);
  }
  if (ellipsisLoc.isInvalid()) {
    // If we don't have an ellipsis the ellipsis index must point after the last
    // element for TupleTypeRepr to be valid.
    ellipsisIdx = tupleElements.size();
  }

  return TupleTypeRepr::create(Context, tupleElements,
                               {leftParenLoc, rightParenLoc}, ellipsisLoc,
                               ellipsisIdx);
}

TypeAttributes ASTGen::generateTypeAttributes(const AttributeListSyntax &Syntax,
                                              const SourceLoc Loc) {
  TypeAttributes attrs;

  for (auto elem : Syntax) {
    // We don't have custom type attributes, only custom decl attributes.
    auto attrSyntax = elem.castTo<AttributeSyntax>();

    auto attrName = attrSyntax.getAttributeName().getText();

    // If we haven't recorded the location of the first '@' yet, do so now.
    auto atLoc = advanceLocBegin(Loc, attrSyntax.getAtSignToken());
    if (attrs.AtLoc.isInvalid()) {
      attrs.AtLoc = atLoc;
    }

    auto attr = TypeAttributes::getAttrKindFromString(attrName);
    if (attr == TAK_Count) {
      continue;
    }

    if (attrs.has(attr)) {
      P.diagnose(atLoc, diag::duplicate_attribute, /*isModifier=*/false);
      continue;
    }

    // In the following we have two methods of ignoring an attribute:
    // Either 'continue' to continue the loop and don't add the attribute to
    // attrs or 'break' to break the switch (possibly skipping custom handling
    // logic) but still adding it to attrs afterwards.
    switch (attr) {
    case TAK_sil_weak:
    case TAK_sil_unowned:
      if (attrs.hasOwnership()) {
        P.diagnose(atLoc, diag::duplicate_attribute, /*isModifier=*/false);
      }
      break;
    case TAK_opened: {
      // @opened("01234567-89ab-cdef-0123-111111111111")
      auto arg = attrSyntax.getArgument();
      if (!arg) {
        continue;
      }

      assert(arg->castTo<TokenSyntax>().getTokenKind() == tok::string_literal);
      auto tokText = arg->castTo<TokenSyntax>().getText();
      // Remove quotes from the string literal.
      auto literalText = tokText.slice(1, tokText.size() - 1);
      if (auto openedID = UUID::fromString(literalText.str().c_str())) {
        attrs.OpenedID = openedID;
      } else {
        auto argLoc = advanceLocBegin(Loc, *arg);
        P.diagnose(argLoc, diag::opened_attribute_id_value);
      }
      break;
    }
    case TAK_differentiable: {
      auto arg = attrSyntax.getArgument();
      if (!arg) {
        // We don't have an argument. Since an argument is optional for
        // @differentiable, add it anyway, just don't set attrs.linear
        break;
      }
      auto argText = arg->castTo<TokenSyntax>().getIdentifierText();
      if (argText == "linear") {
        attrs.linear = true;
      } else {
        auto argLoc = advanceLocBegin(Loc, *arg);
        P.diagnose(argLoc, diag::attr_differentiable_unexpected_argument,
                   argText);
      }
      break;
    }
    case TAK_convention: {
      // @convention(block)
      // @convention(witness_method: ProtocolName)
      // @convention(c, cType: "void *(void)")
      TypeAttributes::Convention convention;

      auto arg = attrSyntax.getArgument();
      if (!arg) {
        continue;
      }

      if (auto conventionNameTok = arg->getAs<TokenSyntax>()) {
        auto conventionName = conventionNameTok->getIdentifierText();
        // Make an identifier for the convention name and use its string so the
        // name stays alive even after the syntax tree has been destructed.
        convention.Name = Context.getIdentifier(conventionName).str();
      } else if (auto conventionAttributeArgs =
                     arg->getAs<CTypeConventionAttributeArgumentsSyntax>()) {
        auto conventionName = conventionAttributeArgs->getConvention().getIdentifierText();
        conventionName = Context.getIdentifier(conventionName).str();
        convention.Name = conventionName;
        if (auto cType = conventionAttributeArgs->getCType()) {
          // If the attribute doesn't have a cType, this has already been
          // diagnosed in the parser
          auto cTypeToken = *cType;
          assert(cTypeToken.getTokenKind() == tok::string_literal);
          auto cTypeTokenText = cTypeToken.getText();
          auto cTypeString = cTypeTokenText.slice(1, cTypeTokenText.size() - 1);
          cTypeString = Context.getIdentifier(cTypeString).str();
          auto cTypeStringLoc = advanceLocBegin(Loc, cTypeToken);
          convention.ClangType = {cTypeString, cTypeStringLoc};
        }
      } else if (auto witness =
                     arg->getAs<NamedAttributeStringArgumentSyntax>()) {
        assert(witness->getNameTok().getIdentifierText() == "witness_method");
        if (witness->getStringOrDeclname().isMissing()) {
          continue;
        }
        auto protocolName =
            witness->getStringOrDeclname().castTo<DeclNameSyntax>();
        convention.Name = "witness_method";
        convention.WitnessMethodProtocol = generateDeclNameRef(protocolName, Loc);
      } else {
        // Unknown attribute. Ignore.
        continue;
      }
      attrs.ConventionArguments = convention;
      break;
    }
    case TAK__opaqueReturnTypeOf: {
      auto arg = attrSyntax.getArgument();
      // @_opaqueReturnTypeOf("$sMangledName", 0)
      if (!arg) {
        continue;
      }

      auto opaqueArg =
          arg->castTo<OpaqueReturnTypeOfAttributeArgumentsSyntax>();

      auto manglingTok = opaqueArg.getMangledName();
      auto indexTok = opaqueArg.getIndex();

      auto tokText = manglingTok.getText();
      auto mangling = tokText.slice(1, tokText.size() - 1);
      mangling = Context.getIdentifier(mangling).str();
      unsigned index;
      if (indexTok.getText().getAsInteger(10, index)) {
        continue;
      }
      attrs.setOpaqueReturnTypeOf(mangling, index);
      break;
    }
    default: // No special handling for this attribute
      break;
    }

    attrs.setAttr(attr, atLoc);
  }

  return attrs;
}

template <typename T>
ComponentIdentTypeRepr *ASTGen::generateTypeIdentifier(const T &TypeSyntax,
                                                       const SourceLoc Loc) {
  auto declNameLoc = DeclNameLoc(advanceLocBegin(Loc, TypeSyntax.getName()));
  auto declNameRef = DeclNameRef(
      Context.getIdentifier(TypeSyntax.getName().getIdentifierText()));
  if (auto clause = TypeSyntax.getGenericArgumentClause()) {
    SourceLoc lAngleLoc, rAngleLoc;
    SmallVector<TypeRepr *, 4> args;
    generateGenericArgs(*clause, Loc, lAngleLoc, rAngleLoc, args);
    if (!args.empty()) {
      return GenericIdentTypeRepr::create(Context, declNameLoc, declNameRef,
                                          args, {lAngleLoc, rAngleLoc});
    }
  }
  return new (Context) SimpleIdentTypeRepr(declNameLoc, declNameRef);
}

void ASTGen::gatherTypeIdentifierComponents(
    const TypeSyntax &Component, const SourceLoc Loc,
    SmallVectorImpl<ComponentIdentTypeRepr *> &Components) {
  if (auto simpleIdentifier = Component.getAs<SimpleTypeIdentifierSyntax>()) {
    auto componentType = generateTypeIdentifier(*simpleIdentifier, Loc);
    Components.push_back(componentType);
  } else if (auto memberIdentifier =
                 Component.getAs<MemberTypeIdentifierSyntax>()) {
    gatherTypeIdentifierComponents(memberIdentifier->getBaseType(), Loc,
                                   Components);
    auto ComponentType = generateTypeIdentifier(*memberIdentifier, Loc);
    Components.push_back(ComponentType);
  } else {
    llvm_unreachable("unexpected type identifier component");
  }
}

TypeRepr *ASTGen::recoverOldStyleProtocolComposition(
    const syntax::UnknownTypeSyntax &Type, const SourceLoc Loc) {
  auto ChildrenCount = Type.getNumChildren();

  // Can't be old-style protocol composition because we need at least
  // 'protocol' '<'
  if (ChildrenCount < 2) {
    return nullptr;
  }

  auto keyword = Type.getChild(0)->getAs<TokenSyntax>();
  if (!keyword || keyword->getText() != "protocol") {
    return nullptr;
  }
  auto lAngle = Type.getChild(1)->getAs<TokenSyntax>();
  if (!lAngle || lAngle->getTokenKind() != tok::l_angle) {
    return nullptr;
  }

  SmallVector<TypeRepr *, 4> protocols;
  for (unsigned i = 2; i < Type.getNumChildren(); i++) {
    if (auto elem = Type.getChild(i)->getAs<TypeSyntax>()) {
      if (auto proto = generate(*elem, Loc)) {
        protocols.push_back(proto);
      }
    }
  }

  auto keywordLoc = advanceLocBegin(Loc, *keyword);
  auto lAngleLoc = advanceLocBegin(Loc, *lAngle);
  auto endLoc = advanceLocBegin(Loc, *Type.getChild(ChildrenCount - 1));
  return CompositionTypeRepr::create(Context, protocols, keywordLoc,
                                     {lAngleLoc, endLoc});
}
