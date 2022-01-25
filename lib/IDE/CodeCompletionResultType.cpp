//===--- CodeCompletionResultType.cpp -------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/IDE/CodeCompletionResultType.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/USRGeneration.h"
#include "swift/Sema/IDETypeChecking.h"

using namespace swift;
using namespace ide;
using TypeRelation = CodeCompletionResultTypeRelation;

// MARK: - USRBasedTypeContext

USRBasedTypeContext::USRBasedTypeContext(const ExpectedTypeContext &TypeContext,
                                         const DeclContext *DC,
                                         USRBasedTypeArena &Arena)
    : VoidType(Arena.getVoidType(DC->getASTContext())) {

  for (auto possibleTy : TypeContext.possibleTypes) {
    ContextualTypes.emplace_back(USRBasedType::fromType(possibleTy, Arena),
                                 /*IsConvertible=*/false);

    // Add the unwrapped optional types as 'convertible' contextual types.
    auto UnwrappedOptionalType = possibleTy->getOptionalObjectType();
    while (UnwrappedOptionalType) {
      ContextualTypes.emplace_back(
          USRBasedType::fromType(UnwrappedOptionalType, Arena),
          /*IsConvertible=*/true);
      UnwrappedOptionalType = UnwrappedOptionalType->getOptionalObjectType();
    }

    // If the contextual type is an opaque return type, make the protocol a
    // contextual type. E.g. func foo() -> some View { #^COMPLETE^# }
    // should show items conforming to `View` as convertible.
    if (auto OpaqueType = possibleTy->getAs<ArchetypeType>()) {
      llvm::SmallVector<USRBasedType *, 1> USRConforms;
      for (auto Proto : OpaqueType->getConformsTo()) {
        USRConforms.push_back(
            USRBasedType::fromType(Proto->getDeclaredInterfaceType(), Arena));
      }
      if (!USRConforms.empty()) {
        // Archetypes are also be used to model generic return types. In those
        // cases simply ignore them.
        ContextualTypes.emplace_back(USRConforms, /*IsConvertible=*/false);
      }
    }
  }
}

TypeRelation
USRBasedTypeContext::typeRelation(const USRBasedType *ResultType) const {
  if (ResultType == this->VoidType) {
    // Void is not convertible to anything and we don't report Void <-> Void
    // identical matches (see USRBasedType::typeRelation). So we don't have to
    // check anything if the result returns Void.
    return TypeRelation::Unknown;
  }

  TypeRelation Res = TypeRelation::Unknown;
  for (auto &ContextualType : ContextualTypes) {
    Res = std::max(Res, ContextualType.typeRelation(ResultType, VoidType));
  }
  return Res;
}

// MARK: - USRBasedTypeArena

USRBasedType *USRBasedTypeArena::getVoidType(const ASTContext &ASTCtx) {
  if (!VoidType) {
    VoidType = USRBasedType::fromType(ASTCtx.getVoidType(), *this);
  }
  return VoidType;
}

// MARK: - USRBasedType

USRBasedType *USRBasedType::null(USRBasedTypeArena &Arena) {
  return USRBasedType::fromUSR(/*USR=*/"", /*Supertypes=*/{}, Arena);
}

USRBasedType *USRBasedType::fromUSR(StringRef USR,
                                    ArrayRef<USRBasedType *> Supertypes,
                                    USRBasedTypeArena &Arena) {
  auto ExistingTypeIt = Arena.CanonicalTypes.find(USR);
  if (ExistingTypeIt != Arena.CanonicalTypes.end()) {
    assert(ArrayRef<USRBasedType *>(ExistingTypeIt->second->Supertypes) ==
               Supertypes &&
           "Same USR but different supertypes?");
    return ExistingTypeIt->second;
  }
  USRBasedType *Result = new (Arena.Allocator) USRBasedType(USR, Supertypes);
  Arena.CanonicalTypes[USR] = Result;
  return Result;
}

USRBasedType *USRBasedType::fromType(Type Ty, USRBasedTypeArena &Arena) {
  // FIXME: We can't mangle archetypes, so we treat them as null USRBasedTypes.
  if (!Ty || Ty->hasArchetype()) {
    return USRBasedType::null(Arena);
  }
  // USRBasedTypes are backed by canonical types so that equivalent types have
  // the same USR.
  Ty = Ty->getCanonicalType();

  SmallVector<USRBasedType *, 2> Supertypes;
  if (auto Nominal = Ty->getAnyNominal()) {
    // Sorted conformances so we get a deterministic supertype order and can
    // assert that USRBasedTypes with the same USR have the same supertypes.
    auto Conformances = Nominal->getAllConformances(/*sorted=*/true);
    Supertypes.reserve(Conformances.size());
    for (auto Conformance : Conformances) {
      if (Conformance->getDeclContext()->getParentModule() !=
          Nominal->getModuleContext()) {
        // Only include conformances that are declared within the module of the
        // type to avoid caching retractive conformances which might no longer
        // exist when reusing the code completion cache from a different module.
        continue;
      }
      Supertypes.push_back(USRBasedType::fromType(
          Conformance->getProtocol()->getDeclaredInterfaceType(), Arena));
    }
  }
  Type Superclass = Ty->getSuperclass();
  while (Superclass) {
    Supertypes.push_back(USRBasedType::fromType(Superclass, Arena));
    Superclass = Superclass->getSuperclass();
  }

  SmallString<32> USR;
  llvm::raw_svector_ostream OS(USR);
  printTypeUSR(Ty, OS);

  return USRBasedType::fromUSR(USR, Supertypes, Arena);
}

TypeRelation USRBasedType::typeRelation(const USRBasedType *ResultType,
                                        const USRBasedType *VoidType) const {
  // `this` is the contextual type.
  if (this == VoidType) {
    // We don't report Void <-> Void matches because that would boost
    // methods returning Void in e.g.
    // func foo() { #^COMPLETE^# }
    // because #^COMPLETE^# is implicitly returned. But that's not very
    // helpful.
    return TypeRelation::Unknown;
  }
  if (ResultType == this) {
    return TypeRelation::Identical;
  }
  for (auto *Supertype : ResultType->getSupertypes()) {
    if (Supertype == this) {
      return TypeRelation::Convertible;
    }
  }
  return TypeRelation::Unknown;
}

TypeRelation USRBasedTypeContext::ContextualType::typeRelation(
    const USRBasedType *ResultType, const USRBasedType *VoidType) const {
  assert(!Types.empty() && "A contextual type should have at least one type");

  TypeRelation Result = TypeRelation::Identical;
  for (auto ContextType : Types) {
    Result = std::min(Result, ContextType->typeRelation(ResultType, VoidType));
  }
  if (IsConvertible && Result == TypeRelation::Identical) {
    Result = TypeRelation::Convertible;
  }
  return Result;
}

// MARK: - CodeCompletionResultType

static TypeRelation calculateTypeRelation(Type Ty, Type ExpectedTy,
                                          const DeclContext *DC) {
  if (Ty.isNull() || ExpectedTy.isNull() || Ty->is<ErrorType>() ||
      ExpectedTy->is<ErrorType>())
    return TypeRelation::Unrelated;

  // Equality/Conversion of GenericTypeParameterType won't account for
  // requirements – ignore them
  if (!Ty->hasTypeParameter() && !ExpectedTy->hasTypeParameter()) {
    if (Ty->isEqual(ExpectedTy))
      return TypeRelation::Identical;
    bool isAny = false;
    isAny |= ExpectedTy->isAny();
    isAny |= ExpectedTy->is<ArchetypeType>() &&
             !ExpectedTy->castTo<ArchetypeType>()->hasRequirements();

    if (!isAny && isConvertibleTo(Ty, ExpectedTy, /*openArchetypes=*/true,
                                  *const_cast<DeclContext *>(DC)))
      return TypeRelation::Convertible;
  }
  if (auto FT = Ty->getAs<AnyFunctionType>()) {
    if (FT->getResult()->isVoid())
      return TypeRelation::Invalid;
  }
  return TypeRelation::Unrelated;
}

static TypeRelation
calculateMaxTypeRelation(Type Ty, const ExpectedTypeContext &typeContext,
                         const DeclContext *DC) {
  if (Ty->isVoid() && typeContext.requiresNonVoid())
    return TypeRelation::Invalid;
  if (typeContext.empty())
    return TypeRelation::Unknown;

  if (auto funcTy = Ty->getAs<AnyFunctionType>())
    Ty = funcTy->removeArgumentLabels(1);

  auto Result = TypeRelation::Unrelated;
  for (auto expectedTy : typeContext.possibleTypes) {
    // Do not use Void type context for a single-expression body, since the
    // implicit return does not constrain the expression.
    //
    //     { ... -> ()  in x } // x can be anything
    //
    // This behaves differently from explicit return, and from non-Void:
    //
    //     { ... -> Int in x }        // x must be Int
    //     { ... -> ()  in return x } // x must be Void
    if (typeContext.isImplicitSingleExpressionReturn && expectedTy->isVoid())
      continue;

    Result = std::max(Result, calculateTypeRelation(Ty, expectedTy, DC));
  }

  // Map invalid -> unrelated when in a single-expression body, since the
  // input may be incomplete.
  if (typeContext.isImplicitSingleExpressionReturn &&
      Result == TypeRelation::Invalid)
    Result = TypeRelation::Unrelated;

  return Result;
}

llvm::SmallVector<USRBasedType *, 2>
CodeCompletionResultType::getUSRBasedResultTypes(
    USRBasedTypeArena &Arena) const {
  llvm::SmallVector<USRBasedType *, 2> USRBasedTypes;
  USRBasedTypes.reserve(ResultTypes.size());
  if (USRArena) {
    for (auto ResultType : ResultTypes) {
      USRBasedTypes.push_back(ResultType.get<USRBasedType *>());
    }
  } else {
    for (auto ResultType : ResultTypes) {
      USRBasedTypes.push_back(
          USRBasedType::fromType(ResultType.get<Type>(), Arena));
    }
  }
  return USRBasedTypes;
}

TypeRelation CodeCompletionResultType::calculateTypeRelation(
    const ExpectedTypeContext &TypeContext, const DeclContext *DC,
    USRBasedTypeContext *USRTypeContext) const {
  if (isNotApplicable()) {
    return TypeRelation::NotApplicable;
  }

  if (!DC) {
    return TypeRelation::Unknown;
  }

  TypeRelation Res = TypeRelation::Unknown;
  if (USRArena) {
    Optional<USRBasedTypeContext> USRTypeContextStorage;
    if (!USRTypeContext) {
      // If we don't have a USRBasedTypeContext, compute one now.
      USRTypeContextStorage.emplace(TypeContext, DC, *USRArena);
      USRTypeContext = USRTypeContextStorage.getPointer();
    }
    for (auto ResultTy : ResultTypes) {
      Res = std::max(
          Res, USRTypeContext->typeRelation(ResultTy.get<USRBasedType *>()));
    }
  } else {
    for (auto ResultTy : ResultTypes) {
      Res = std::max(
          Res, calculateMaxTypeRelation(ResultTy.get<Type>(), TypeContext, DC));
    }
  }
  return Res;
}
