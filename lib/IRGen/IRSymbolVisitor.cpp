//===--- IRSymbolVisitor.cpp - IR Linker Symbol Visitor ------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements liker symbol enumeration for IRSymbolVisitor.
//
//===----------------------------------------------------------------------===//

#include "swift/IRGen/IRSymbolVisitor.h"

#include "swift/Basic/CodeGenerationModel.h"
#include "swift/SIL/SILSymbolVisitor.h"

using namespace swift;
using namespace irgen;

/// Determine whether dynamic replacement should be emitted for the allocator or
/// the initializer given a decl. The rule is that structs and convenience init
/// of classes emit a dynamic replacement for the allocator. A designated init
/// of a class emits a dynamic replacement for the initializer. This is because
/// the super class init call is emitted to the initializer and needs to be
/// dynamic.
static bool shouldUseAllocatorMangling(const AbstractFunctionDecl *AFD) {
  auto constructor = dyn_cast<ConstructorDecl>(AFD);
  if (!constructor)
    return false;
  return constructor->getParent()->getSelfClassDecl() == nullptr ||
         constructor->isConvenienceInit();
}

/// Determine whether values of the given type are known to occupy no storage
/// in Embedded Swift, where IRGen does not emit storage for them. Embedded
/// Swift has no resilience, so this is structural. Answers conservatively:
/// "false" when unsure.
static bool isKnownEmptyInEmbedded(Type type) {
  if (auto tuple = type->getAs<TupleType>()) {
    return llvm::all_of(tuple->getElementTypes(),
                        [](Type elt) { return isKnownEmptyInEmbedded(elt); });
  }

  auto *nominal = type->getAnyNominal();
  if (!nominal || nominal->hasClangNode() ||
      nominal->getAttrs().hasAttribute<RawLayoutAttr>())
    return false;

  if (auto *structDecl = dyn_cast<StructDecl>(nominal)) {
    return llvm::all_of(structDecl->getStoredProperties(), [&](VarDecl *var) {
      return isKnownEmptyInEmbedded(type->getTypeOfMember(var));
    });
  }

  if (auto *enumDecl = dyn_cast<EnumDecl>(nominal)) {
    if (enumDecl->isIndirect() || enumDecl->isGenericContext())
      return false;

    // An enum is empty when it has at most one case, and that case has no
    // payload or an empty one.
    auto elements = enumDecl->getAllElements();
    if (elements.empty())
      return true;
    if (std::next(elements.begin()) != elements.end())
      return false;
    auto *element = *elements.begin();
    if (element->isIndirect())
      return false;
    auto payload = element->getPayloadInterfaceType();
    return !payload || isKnownEmptyInEmbedded(payload);
  }

  return false;
}

/// The underlying implementation of the IR symbol visitor logic. This class is
/// responsible for overriding the abstract methods of `SILSymbolVisitor` and
/// emitting `LinkEntity` instances to the downstream visitor in addition to
/// passing through `SILDeclRef`s produced by the SIL symbol visitor.
class IRSymbolVisitorImpl : public SILSymbolVisitor {
  IRSymbolVisitor &Visitor;
  const IRSymbolVisitorContext &Ctx;
  bool PublicOrPackageSymbolsOnly;

  /// Whether symbols are being enumerated for Embedded Swift, which emits only
  /// a small subset of the entities of the full Swift ABI, and gives strong
  /// definitions only to those with a unique definition in this module.
  bool IsEmbedded;

  /// Emits the given `LinkEntity` to the downstream visitor as long as the
  /// entity has the required linkage.
  ///
  /// FIXME: The need for an ignoreVisibility flag here possibly indicates that
  ///        there is something broken about the linkage computation below.
  void addLinkEntity(LinkEntity entity, bool ignoreVisibility = false) {
    // Embedded Swift only produces strong definitions, so always check the
    // linkage there.
    if (!ignoreVisibility || IsEmbedded) {
      auto linkage =
          LinkInfo::get(Ctx.getLinkInfo(), Ctx.getSILCtx().getModule(), entity,
                        ForDefinition);

      auto externallyVisible =
          llvm::GlobalValue::isExternalLinkage(linkage.getLinkage()) &&
          linkage.getVisibility() != llvm::GlobalValue::HiddenVisibility;

      if ((PublicOrPackageSymbolsOnly || IsEmbedded) && !externallyVisible)
        return;
    }

    Visitor.addLinkEntity(entity);
  }

  /// Emits the `LinkEntity` produced by `getEntity`, which is part of the
  /// runtime ABI of non-Embedded Swift. Embedded Swift never emits these
  /// entities, and many of them cannot even be formed there.
  void addNonEmbeddedLinkEntity(llvm::function_ref<LinkEntity()> getEntity) {
    if (IsEmbedded)
      return;

    addLinkEntity(getEntity());
  }

public:
  IRSymbolVisitorImpl(IRSymbolVisitor &Visitor,
                      const IRSymbolVisitorContext &Ctx)
      : Visitor{Visitor}, Ctx{Ctx},
        PublicOrPackageSymbolsOnly{
            Ctx.getSILCtx().getOpts().PublicOrPackageSymbolsOnly},
        IsEmbedded{
            Ctx.getSILCtx().getModule()->getASTContext().LangOpts.hasFeature(
                Feature::Embedded)} {}

  bool willVisitDecl(Decl *D) override {
    return Visitor.willVisitDecl(D);
  }

  void didVisitDecl(Decl *D) override {
    Visitor.didVisitDecl(D);
  }

  void addAssociatedConformanceDescriptor(AssociatedConformance AC) override {
    addNonEmbeddedLinkEntity(
        [&] { return LinkEntity::forAssociatedConformanceDescriptor(AC); });
  }

  void addAssociatedTypeDescriptor(AssociatedTypeDecl *ATD) override {
    addNonEmbeddedLinkEntity(
        [&] { return LinkEntity::forAssociatedTypeDescriptor(ATD); });
  }

  void addAsyncFunctionPointer(SILDeclRef declRef) override {
    addLinkEntity(LinkEntity::forAsyncFunctionPointer(declRef),
                  /*ignoreVisibility=*/true);
  }

  void addBaseConformanceDescriptor(BaseConformance BC) override {
    addNonEmbeddedLinkEntity(
        [&] { return LinkEntity::forBaseConformanceDescriptor(BC); });
  }

  void addClassMetadataBaseOffset(ClassDecl *CD) override {
    addNonEmbeddedLinkEntity(
        [&] { return LinkEntity::forClassMetadataBaseOffset(CD); });
  }

  void addCoroFunctionPointer(SILDeclRef declRef) override {
    addLinkEntity(LinkEntity::forCoroFunctionPointer(declRef),
                  /*ignoreVisibility=*/true);
  }

  void addDispatchThunk(SILDeclRef declRef) override {
    // Embedded Swift does not have dispatch thunks.
    if (IsEmbedded)
      return;

    auto entity = LinkEntity::forDispatchThunk(declRef);

    addLinkEntity(entity);

    if (declRef.getAbstractFunctionDecl()->hasAsync())
      addLinkEntity(LinkEntity::forAsyncFunctionPointer(entity));

    auto *accessor = dyn_cast<AccessorDecl>(declRef.getAbstractFunctionDecl());
    if (accessor &&
        requiresFeatureCoroutineAccessors(accessor->getAccessorKind()))
      addLinkEntity(LinkEntity::forCoroFunctionPointer(entity));
  }

  void addDynamicFunction(AbstractFunctionDecl *AFD,
                          DynamicKind dynKind) override {
    // Embedded Swift does not support dynamic replacement.
    if (IsEmbedded)
      return;

    bool useAllocator = shouldUseAllocatorMangling(AFD);
    addLinkEntity(LinkEntity::forDynamicallyReplaceableFunctionVariable(
        AFD, useAllocator));
    switch (dynKind) {
    case DynamicKind::Replaceable:
      addLinkEntity(
          LinkEntity::forDynamicallyReplaceableFunctionKey(AFD, useAllocator));
      break;
    case DynamicKind::Replacement:
      addLinkEntity(
          LinkEntity::forDynamicallyReplaceableFunctionImpl(AFD, useAllocator));
      break;
    }
  }

  void addEnumCase(EnumElementDecl *EED) override {
    addNonEmbeddedLinkEntity([&] { return LinkEntity::forEnumCase(EED); });
  }

  void addFieldOffset(VarDecl *VD) override {
    addNonEmbeddedLinkEntity([&] { return LinkEntity::forFieldOffset(VD); });
  }

  void addFunction(SILDeclRef declRef) override {
    // In Embedded Swift, a function without a unique definition in this module
    // is emitted on demand into each module that uses it.
    if (IsEmbedded && declRef.hasNonUniqueDefinition())
      return;

    Visitor.addFunction(declRef);
  }

  void addFunction(StringRef name, SILDeclRef declRef) override {
    Visitor.addFunction(name, declRef);
  }

  void addGlobalVar(VarDecl *VD) override {
    // In Embedded Swift, a global variable without a unique definition in this
    // module is emitted on demand into each module that uses it.
    if (IsEmbedded && SILDeclRef::declHasNonUniqueDefinition(VD))
      return;

    // Embedded Swift does not emit storage for an empty global variable.
    if (IsEmbedded && isKnownEmptyInEmbedded(VD->getInterfaceType()))
      return;

    Visitor.addGlobalVar(VD);
  }

  void addMethodDescriptor(SILDeclRef declRef) override {
    addNonEmbeddedLinkEntity(
        [&] { return LinkEntity::forMethodDescriptor(declRef); });
  }

  void addMethodLookupFunction(ClassDecl *CD) override {
    addNonEmbeddedLinkEntity(
        [&] { return LinkEntity::forMethodLookupFunction(CD); });
  }

  void addNominalTypeDescriptor(NominalTypeDecl *NTD) override {
    addNonEmbeddedLinkEntity(
        [&] { return LinkEntity::forNominalTypeDescriptor(NTD); });
  }

  void addObjCInterface(ClassDecl *CD) override {
    Visitor.addObjCInterface(CD);
  }

  void addObjCMetaclass(ClassDecl *CD) override {
    addNonEmbeddedLinkEntity([&] { return LinkEntity::forObjCMetaclass(CD); });
  }

  void addObjCMethod(AbstractFunctionDecl *AFD) override {
    // Pass through; Obj-C methods don't have linkable symbols.
    Visitor.addObjCMethod(AFD);
  }

  void addObjCResilientClassStub(ClassDecl *CD) override {
    addNonEmbeddedLinkEntity([&] {
      return LinkEntity::forObjCResilientClassStub(
          CD, TypeMetadataAddress::AddressPoint);
    });
  }

  void addOpaqueTypeDescriptor(OpaqueTypeDecl *OTD) override {
    addNonEmbeddedLinkEntity(
        [&] { return LinkEntity::forOpaqueTypeDescriptor(OTD); });
  }

  void addOpaqueTypeDescriptorAccessor(OpaqueTypeDecl *OTD,
                                       DynamicKind dynKind) override {
    // Embedded Swift does not have opaque type descriptors.
    if (IsEmbedded)
      return;

    addLinkEntity(LinkEntity::forOpaqueTypeDescriptorAccessor(OTD));
    switch (dynKind) {
    case DynamicKind::Replaceable:
      addLinkEntity(LinkEntity::forOpaqueTypeDescriptorAccessorImpl(OTD));
      addLinkEntity(LinkEntity::forOpaqueTypeDescriptorAccessorKey(OTD));
      break;
    case DynamicKind::Replacement:
      break;
    }
    addLinkEntity(LinkEntity::forOpaqueTypeDescriptorAccessorVar(OTD));
  }

  void addPropertyDescriptor(AbstractStorageDecl *ASD) override {
    addNonEmbeddedLinkEntity(
        [&] { return LinkEntity::forPropertyDescriptor(ASD); });
  }

  void addProtocolConformanceDescriptor(RootProtocolConformance *C) override {
    addNonEmbeddedLinkEntity(
        [&] { return LinkEntity::forProtocolConformanceDescriptor(C); });
  }

  void addProtocolDescriptor(ProtocolDecl *PD) override {
    addNonEmbeddedLinkEntity(
        [&] { return LinkEntity::forProtocolDescriptor(PD); });
  }

  void addProtocolRequirementsBaseDescriptor(ProtocolDecl *PD) override {
    addNonEmbeddedLinkEntity(
        [&] { return LinkEntity::forProtocolRequirementsBaseDescriptor(PD); });
  }

  void addProtocolWitnessTable(RootProtocolConformance *C) override {
    addLinkEntity(LinkEntity::forProtocolWitnessTable(C));
  }

  void addProtocolWitnessThunk(RootProtocolConformance *C,
                               ValueDecl *requirementDecl) override {
    // Embedded Swift emits witness thunks on demand into each module that uses
    // them.
    if (IsEmbedded)
      return;

    Visitor.addProtocolWitnessThunk(C, requirementDecl);
  }

  void addCOMMethodWitnessThunk(RootProtocolConformance *C,
                                ValueDecl *requirementDecl) override {
    Visitor.addCOMMethodWitnessThunk(C, requirementDecl);
  }

  void addSwiftMetaclassStub(ClassDecl *CD) override {
    addNonEmbeddedLinkEntity(
        [&] { return LinkEntity::forSwiftMetaclassStub(CD); });
  }

  void addTypeMetadataAccessFunction(CanType T) override {
    addNonEmbeddedLinkEntity(
        [&] { return LinkEntity::forTypeMetadataAccessFunction(T); });
  }

  void addTypeMetadataAddress(CanType T) override {
    if (IsEmbedded) {
      // Embedded Swift eagerly emits metadata only for @export(interface)
      // types, for which both the full metadata and the alias to its address
      // point are strong definitions. Other metadata is emitted on demand.
      auto *nominal = T->getAnyNominal();
      if (!nominal || nominal->getEffectiveCodeGenerationModel() !=
                          CodeGenerationModel::Interface)
        return;

      addLinkEntity(
          LinkEntity::forTypeMetadata(T, TypeMetadataAddress::FullMetadata));
    }

    addLinkEntity(
        LinkEntity::forTypeMetadata(T, TypeMetadataAddress::AddressPoint));
  }
};

void IRSymbolVisitor::visit(Decl *D, const IRSymbolVisitorContext &Ctx) {
  IRSymbolVisitorImpl(*this, Ctx).visitDecl(D, Ctx.getSILCtx());
}

void IRSymbolVisitor::visitFile(FileUnit *file,
                                const IRSymbolVisitorContext &Ctx) {
  IRSymbolVisitorImpl(*this, Ctx).visitFile(file, Ctx.getSILCtx());
}

void IRSymbolVisitor::visitModules(llvm::SmallVector<ModuleDecl *, 4> &modules,
                                   const IRSymbolVisitorContext &Ctx) {
  IRSymbolVisitorImpl(*this, Ctx).visitModules(modules, Ctx.getSILCtx());
}
