//===--- ClassAllocatorWitnesses.cpp - Class allocator witnesses ----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/ClassAllocatorWitnesses.h"
#include "swift/AST/ActorIsolation.h"
#include "swift/AST/ConcreteDeclRef.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SourceFile.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILLinkage.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/TypeLowering.h"

using namespace swift;

FuncDecl *swift::getClassAllocatorDeallocateRequirement(ASTContext &ctx) {
  auto *proto = ctx.getProtocol(KnownProtocolKind::ClassAllocator);
  if (!proto)
    return nullptr;
  for (ValueDecl *member :
       proto->lookupDirect(DeclName(ctx.getIdentifier("deallocate")))) {
    auto *fn = dyn_cast<FuncDecl>(member);
    if (fn && fn->isProtocolRequirement())
      return fn;
  }
  return nullptr;
}

ConcreteDeclRef
swift::getDirectlyCallableClassAllocatorDeallocate(SILModule &M,
                                                   CanType allocatorType) {
  ASTContext &ctx = M.getASTContext();
  auto *requirement = getClassAllocatorDeallocateRequirement(ctx);
  if (!requirement)
    return ConcreteDeclRef();
  auto conformance =
      lookupConformance(allocatorType, cast<ProtocolDecl>(requirement->getDeclContext()));
  if (!conformance.isConcrete())
    return ConcreteDeclRef();
  ConcreteDeclRef witness =
      conformance.getConcrete()->getWitnessDeclRef(requirement);
  auto *fn = dyn_cast_or_null<FuncDecl>(witness.getDecl());
  if (!fn || fn->hasAsync() || fn->hasThrows() || fn->isDynamic() ||
      fn->isBackDeployed() || fn->shouldUseObjCDispatch())
    return ConcreteDeclRef();
  if (getActorIsolation(fn).isActorIsolated())
    return ConcreteDeclRef();
  // A class method that a subclass may override is dispatched through the
  // class's vtable.
  if (auto *cls = fn->getDeclContext()->getSelfClassDecl())
    if (!fn->isFinal() && !cls->isSemanticallyFinal())
      return ConcreteDeclRef();
  // A private symbol is visible only in its own file.
  if (SILDeclRef(fn).getLinkage(ForDefinition) == SILLinkage::Private) {
    if (fn->getModuleContext() != M.getSwiftModule())
      return ConcreteDeclRef();
    if (!M.isWholeModule() && fn->getDeclContext()->getModuleScopeContext() !=
                                  M.getAssociatedContext())
      return ConcreteDeclRef();
  }
  return witness;
}

std::optional<SILDeclRef>
swift::getClassAllocatorDeallocateMethod(SILModule &M, CanType allocatorType) {
  auto *nominal = allocatorType->getAnyNominal();
  if (!nominal || !isa<StructDecl, EnumDecl>(nominal) ||
      nominal->isGenericContext())
    return std::nullopt;
  ConcreteDeclRef witness =
      getDirectlyCallableClassAllocatorDeallocate(M, allocatorType);
  if (!witness)
    return std::nullopt;
  // Declared on the type itself, not in a protocol extension.
  auto *fn = cast<FuncDecl>(witness.getDecl());
  if (fn->getDeclContext()->getSelfNominalTypeDecl() != nominal ||
      fn->isGenericContext())
    return std::nullopt;

  // Its lowered type must be exactly the entry's: the four parameters of the
  // requirement, passed directly, and the thin metatype of the type, which has
  // no representation, as `self`.
  SILDeclRef method(fn);
  auto fnType = M.Types.getConstantFunctionType(
      TypeExpansionContext::minimal(), method);
  if (fnType->getRepresentation() != SILFunctionTypeRepresentation::Method ||
      fnType->isPolymorphic() || fnType->isAsync() || fnType->isCoroutine() ||
      fnType->hasErrorResult() || fnType->getNumResults() != 0 ||
      fnType->getNumParameters() != 5)
    return std::nullopt;
  ASTContext &ctx = M.getASTContext();
  CanType expected[] = {
      ctx.getUnsafeMutableRawPointerType()->getCanonicalType(),
      ctx.getIntType()->getCanonicalType(),
      ctx.getIntType()->getCanonicalType(),
      CanExistentialMetatypeType::get(ctx.getAnyObjectType(),
                                      MetatypeRepresentation::Thick),
  };
  for (unsigned i = 0; i != 4; ++i) {
    SILParameterInfo param = fnType->getParameters()[i];
    if (isIndirectFormalParameter(param.getConvention()) ||
        param.getOptions() || param.getInterfaceType() != expected[i])
      return std::nullopt;
  }
  SILParameterInfo self = fnType->getSelfParameter();
  auto selfType = dyn_cast<MetatypeType>(self.getInterfaceType());
  if (!selfType || !selfType->hasRepresentation() ||
      selfType->getRepresentation() != MetatypeRepresentation::Thin ||
      selfType.getInstanceType() != allocatorType)
    return std::nullopt;
  return method;
}

SILFunction *swift::lookUpClassAllocatorDeallocateEntry(SILModule &M,
                                                        CanType allocatorType) {
  SILFunction *fn = M.lookUpFunction(getClassAllocatorWitnessName(
      allocatorType, ClassAllocatorWitnessKind::Deallocate));
  if (!fn)
    if (auto method = getClassAllocatorDeallocateMethod(M, allocatorType))
      fn = M.lookUpFunction(*method);
  if (fn && !isReferenceableClassAllocatorFunction(fn))
    return nullptr;
  return fn;
}

bool swift::isReferenceableClassAllocatorFunction(const SILFunction *fn) {
  if (!fn->isExternalDeclaration())
    return true;
  // A function without a symbol, such as an `@_alwaysEmitIntoClient` one, is
  // usable only if this module has its body, and emits it.
  if (fn->isAlwaysEmitIntoClient())
    return false;
  switch (fn->getLinkage()) {
  case SILLinkage::Shared:
  case SILLinkage::PublicNonABI:
  case SILLinkage::PackageNonABI:
    return false;
  default:
    return true;
  }
}
