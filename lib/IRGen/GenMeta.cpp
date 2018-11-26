//===--- GenMeta.cpp - IR generation for metadata constructs --------------===//
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
//  This file implements IR generation for type metadata constructs.
//
//===----------------------------------------------------------------------===//

#include "swift/ABI/MetadataValues.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/CanTypeVisitor.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Attr.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/AST/Types.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/IRGen/Linking.h"
#include "swift/SIL/FormalLinkage.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/TypeLowering.h"
#include "swift/Strings.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Module.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"

#include "Address.h"
#include "Callee.h"
#include "ClassMetadataVisitor.h"
#include "ConstantBuilder.h"
#include "EnumMetadataVisitor.h"
#include "FixedTypeInfo.h"
#include "ForeignClassMetadataVisitor.h"
#include "GenArchetype.h"
#include "GenClass.h"
#include "GenDecl.h"
#include "GenPoly.h"
#include "GenStruct.h"
#include "GenValueWitness.h"
#include "HeapTypeInfo.h"
#include "IRGenDebugInfo.h"
#include "IRGenMangler.h"
#include "IRGenModule.h"
#include "MetadataLayout.h"
#include "MetadataRequest.h"
#include "ProtocolInfo.h"
#include "ScalarTypeInfo.h"
#include "StructLayout.h"
#include "StructMetadataVisitor.h"

#include "GenMeta.h"

using namespace swift;
using namespace irgen;

static Address emitAddressOfMetadataSlotAtIndex(IRGenFunction &IGF,
                                                llvm::Value *metadata,
                                                int index,
                                                llvm::Type *objectTy) {
  // Require the metadata to be some type that we recognize as a
  // metadata pointer.
  assert(metadata->getType() == IGF.IGM.TypeMetadataPtrTy);

  return IGF.emitAddressAtOffset(metadata,
                                 Offset(index * IGF.IGM.getPointerSize()),
                                 objectTy, IGF.IGM.getPointerAlignment());
}

/// Emit a load from the given metadata at a constant index.
static llvm::LoadInst *emitLoadFromMetadataAtIndex(IRGenFunction &IGF,
                                                   llvm::Value *metadata,
                                                   int index,
                                                   llvm::Type *objectTy,
                                             const llvm::Twine &suffix = "") {
  Address slot =
    emitAddressOfMetadataSlotAtIndex(IGF, metadata, index, objectTy);

  // Load.
  return IGF.Builder.CreateLoad(slot, metadata->getName() + suffix);
}

static Address createPointerSizedGEP(IRGenFunction &IGF,
                                     Address base,
                                     Size offset) {
  return IGF.Builder.CreateConstArrayGEP(base,
                                         IGF.IGM.getOffsetInWords(offset),
                                         offset);
}

void IRGenModule::setTrueConstGlobal(llvm::GlobalVariable *var) {
  disableAddressSanitizer(*this, var);
  
  switch (TargetInfo.OutputObjectFormat) {
  case llvm::Triple::UnknownObjectFormat:
    llvm_unreachable("unknown object format");
  case llvm::Triple::MachO:
    var->setSection("__TEXT,__const");
    break;
  case llvm::Triple::ELF:
    var->setSection(".rodata");
    break;
  case llvm::Triple::COFF:
    var->setSection(".rdata");
    break;
  case llvm::Triple::Wasm:
    llvm_unreachable("web assembly object format is not supported.");
    break;
  }
}

/*****************************************************************************/
/** Nominal Type Descriptor Emission *****************************************/
/*****************************************************************************/

template <class Flags>
static Flags getMethodDescriptorFlags(ValueDecl *fn) {
  if (isa<ConstructorDecl>(fn))
    return Flags(Flags::Kind::Init); // 'init' is considered static

  auto kind = [&] {
    auto accessor = dyn_cast<AccessorDecl>(fn);
    if (!accessor) return Flags::Kind::Method;
    switch (accessor->getAccessorKind()) {
    case AccessorKind::IsGetter:
      return Flags::Kind::Getter;
    case AccessorKind::IsSetter:
      return Flags::Kind::Setter;
    case AccessorKind::IsMaterializeForSet:
      return Flags::Kind::MaterializeForSet;
    case AccessorKind::IsWillSet:
    case AccessorKind::IsDidSet:
    case AccessorKind::IsAddressor:
    case AccessorKind::IsMutableAddressor:
      llvm_unreachable("these accessors never appear in protocols or v-tables");
    }
    llvm_unreachable("bad kind");
  }();
  return Flags(kind).withIsInstance(!fn->isStatic());
}

namespace {
  template<class Impl>
  class ContextDescriptorBuilderBase {
  protected:
    Impl &asImpl() { return *static_cast<Impl*>(this); }
    IRGenModule &IGM;
  private:
    ConstantInitBuilder InitBuilder;
  protected:
    ConstantStructBuilder B;
    Optional<ConstantAggregateBuilderBase::PlaceholderPosition>
      GenericParamCount,
      GenericRequirementCount,
      GenericKeyArgumentCount,
      GenericExtraArgumentCount;
    unsigned NumGenericKeyArguments = 0;
    unsigned NumGenericExtraArguments = 0;

    ContextDescriptorBuilderBase(IRGenModule &IGM)
      : IGM(IGM), InitBuilder(IGM), B(InitBuilder.beginStruct()) {
      B.setPacked(true);
    }
  
  public:
    void layout() {
      asImpl().addFlags();
      asImpl().addParent();
    }
    
    void addFlags() {
      B.addInt32(
        ContextDescriptorFlags(asImpl().getContextKind(),
                               asImpl().getGenericSignature() != nullptr,
                               asImpl().isUniqueDescriptor(),
                               asImpl().getVersion(),
                               asImpl().getKindSpecificFlags())
          .getIntValue());
    }
    
    void addParent() {
      ConstantReference parent = asImpl().getParent();
      if (parent.getValue()) {
        B.addRelativeAddress(parent);
      } else {
        B.addInt32(0); // null offset
      }
    }
    
    void addGenericSignature() {
      if (!asImpl().getGenericSignature())
        return;
      
      asImpl().addGenericParametersHeader();
      asImpl().addGenericParameters();
      asImpl().addGenericRequirements();
      asImpl().finishGenericParameters();
    }
    
    void addGenericParametersHeader() {
      // Drop placeholders for the counts. We'll fill these in when we emit
      // the related sections.
      GenericParamCount = B.addPlaceholderWithSize(IGM.Int16Ty);
      GenericRequirementCount = B.addPlaceholderWithSize(IGM.Int16Ty);
      GenericKeyArgumentCount = B.addPlaceholderWithSize(IGM.Int16Ty);
      GenericExtraArgumentCount = B.addPlaceholderWithSize(IGM.Int16Ty);
    }
    
    void addGenericParameters() {
      GenericSignature *sig = asImpl().getGenericSignature();
      assert(sig);
      auto canSig = sig->getCanonicalSignature();
      
      for (auto param : canSig->getGenericParams()) {
        // Currently, there are only type parameters. The parameter is a key
        // argument if it's canonical in its generic context.
        asImpl().addGenericParameter(GenericParamKind::Type,
                 /*key argument*/ canSig->isCanonicalTypeInContext(param),
                 /*extra argument*/ false);
      }
      
      // Pad the structure up to four bytes for the following requirements.
      unsigned padding = (unsigned) -canSig->getGenericParams().size() & 3;
      for (unsigned i = 0; i < padding; ++i)
        B.addInt(IGM.Int8Ty, 0);
      
      // Fill in the parameter count.
      assert(canSig->getGenericParams().size() <= UINT16_MAX
             && "way too generic");
      B.fillPlaceholderWithInt(*GenericParamCount, IGM.Int16Ty,
                               canSig->getGenericParams().size());
    }
    
    void addGenericParameter(GenericParamKind kind,
                             bool isKeyArgument, bool isExtraArgument) {
      if (isKeyArgument)
        ++NumGenericKeyArguments;
      if (isExtraArgument)
        ++NumGenericExtraArguments;
      
      B.addInt(IGM.Int8Ty,
               GenericParamDescriptor(kind, isKeyArgument, isExtraArgument)
                 .getIntValue());
    }
    
    void addGenericRequirements() {
      auto metadata =
        irgen::addGenericRequirements(IGM, B,
                            asImpl().getGenericSignature(),
                            asImpl().getGenericSignature()->getRequirements());

      // Fill in the final requirement count.
      assert(metadata.NumRequirements <= UINT16_MAX
             && "way too generic");
      B.fillPlaceholderWithInt(*GenericRequirementCount, IGM.Int16Ty,
                               metadata.NumRequirements);
      NumGenericKeyArguments += metadata.NumGenericKeyArguments;
      NumGenericExtraArguments += metadata.NumGenericExtraArguments;
    }

    void finishGenericParameters() {
      assert(NumGenericKeyArguments <= UINT16_MAX
             && NumGenericExtraArguments <= UINT16_MAX
             && "way too generic");
      B.fillPlaceholderWithInt(*GenericKeyArgumentCount, IGM.Int16Ty,
                               NumGenericKeyArguments);
      B.fillPlaceholderWithInt(*GenericExtraArgumentCount, IGM.Int16Ty,
                               NumGenericExtraArguments);
    }

    uint8_t getVersion() {
      return 0;
    }
    
    uint16_t getKindSpecificFlags() {
      return 0;
    }
    
    // Subclasses should provide:
    //
    // bool isUniqueDescriptor();
    // llvm::Constant *getParent();
    // ContextDescriptorKind getContextKind();
    // GenericSignature *getGenericSignature();
    // void emit();
  };
  
  class ModuleContextDescriptorBuilder
      : public ContextDescriptorBuilderBase<ModuleContextDescriptorBuilder> {
    using super = ContextDescriptorBuilderBase;
    
    ModuleDecl *M;
    
  public:
    ModuleContextDescriptorBuilder(IRGenModule &IGM, ModuleDecl *M)
      : super(IGM), M(M)
    {}
  
    void layout() {
      super::layout();
      addName();
    }
    
    void addName() {
      B.addRelativeAddress(IGM.getAddrOfGlobalString(M->getName().str(),
                                           /*willBeRelativelyAddressed*/ true));
    }
    
    bool isUniqueDescriptor() {
      return false;
    }
  
    ConstantReference getParent() {
      return {nullptr, ConstantReference::Direct};
    }
    
    ContextDescriptorKind getContextKind() {
      return ContextDescriptorKind::Module;
    }
    
    GenericSignature *getGenericSignature() {
      return nullptr;
    }
        
    void emit() {
      asImpl().layout();
      
      auto addr = IGM.getAddrOfModuleContextDescriptor(M,
                                                     B.finishAndCreateFuture());
      auto var = cast<llvm::GlobalVariable>(addr);
      
      var->setConstant(true);
      IGM.setTrueConstGlobal(var);
    }
  };

  class ExtensionContextDescriptorBuilder
    : public ContextDescriptorBuilderBase<ExtensionContextDescriptorBuilder> {
    
    using super = ContextDescriptorBuilderBase;
    
    ExtensionDecl *E;
  
  public:
    ExtensionContextDescriptorBuilder(IRGenModule &IGM, ExtensionDecl *E)
      : super(IGM), E(E)
    {}
    
    void layout() {
      super::layout();
      addExtendedContext();
      addGenericSignature();
    }
    
    void addExtendedContext() {
      auto string = IGM.getTypeRef(
          E->getSelfInterfaceType()->getCanonicalType());
      B.addRelativeAddress(string);
    }
    
    ConstantReference getParent() {
      return {IGM.getAddrOfModuleContextDescriptor(E->getParentModule()),
              ConstantReference::Direct};
    }
    
    bool isUniqueDescriptor() {
      // Extensions generated by the Clang importer will be emitted into any
      // binary that uses the Clang module. Otherwise, we can guarantee that
      // an extension (and any of its possible sub-contexts) belong to one
      // translation unit.
      return !isa<ClangModuleUnit>(E->getModuleScopeContext());
    }
    
    ContextDescriptorKind getContextKind() {
      return ContextDescriptorKind::Extension;
    }
    
    GenericSignature *getGenericSignature() {
      return E->getGenericSignature();
    }
      
    void emit() {
      asImpl().layout();
      
      auto addr = IGM.getAddrOfExtensionContextDescriptor(E,
                                                     B.finishAndCreateFuture());
      auto var = cast<llvm::GlobalVariable>(addr);
      
      var->setConstant(true);
      IGM.setTrueConstGlobal(var);
    }
  };
  
  class AnonymousContextDescriptorBuilder
    : public ContextDescriptorBuilderBase<AnonymousContextDescriptorBuilder> {
    
    using super = ContextDescriptorBuilderBase;
    
    DeclContext *DC;
  
  public:
    AnonymousContextDescriptorBuilder(IRGenModule &IGM, DeclContext *DC)
      : super(IGM), DC(DC)
    {
    }
    
    void layout() {
      super::layout();
    }
  
    ConstantReference getParent() {
      return {IGM.getAddrOfModuleContextDescriptor(DC->getParentModule()),
              ConstantReference::Direct};
    }
    
    ContextDescriptorKind getContextKind() {
      return ContextDescriptorKind::Anonymous;
    }
    
    GenericSignature *getGenericSignature() {
      return nullptr;
    }
    
    bool isUniqueDescriptor() {
      return true;
    }

    void emit() {
      asImpl().layout();
      auto addr = IGM.getAddrOfAnonymousContextDescriptor(DC,
                                                     B.finishAndCreateFuture());
      auto var = cast<llvm::GlobalVariable>(addr);
      
      var->setConstant(true);
      IGM.setTrueConstGlobal(var);
    }
  };
  
  template<class Impl>
  class TypeContextDescriptorBuilderBase
    : public ContextDescriptorBuilderBase<Impl> {
  
    using super = ContextDescriptorBuilderBase<Impl>;
  
  protected:
    NominalTypeDecl *Type;
    RequireMetadata_t HasMetadata;
    
    using super::IGM;
    using super::B;
    using super::asImpl;

  public:
    using super::addGenericSignature;
  
    TypeContextDescriptorBuilderBase(IRGenModule &IGM, NominalTypeDecl *Type,
                                     RequireMetadata_t requireMetadata)
      : super(IGM), Type(Type),
        HasMetadata(requireMetadata)
    {}
    
    void layout() {
      super::layout();
      asImpl().addName();
      asImpl().addAccessFunction();
      // ABI TODO: layout info should be superseded by remote mirror metadata
      asImpl().addLayoutInfo();
      asImpl().addGenericSignature();
    }
    
    void addName() {
      StringRef name;
      
      // Try to use the Clang name if there is one.
      if (auto namedClangDecl =
                             Mangle::ASTMangler::getClangDeclForMangling(Type)) {
        name = namedClangDecl->getName();
      } else {
        name = Type->getName().str();
      }
      
      auto nameStr = IGM.getAddrOfGlobalString(name,
                                           /*willBeRelativelyAddressed*/ true);
      B.addRelativeAddress(nameStr);
    }
      
    void addAccessFunction() {
      // Don't emit the access function if we're only lazily emitting the
      // context descriptor.
      if (!HasMetadata) {
        B.addInt32(0);
        return;
      }
    
      llvm::Constant *accessFn =
        getRequiredTypeMetadataAccessFunction(IGM, Type, NotForDefinition);
      B.addRelativeAddressOrNull(accessFn);
    }
    
    ConstantReference getParent() {
      return IGM.getAddrOfParentContextDescriptor(Type);
    }
    
    GenericSignature *getGenericSignature() {
      return Type->getGenericSignature();
    }
    
    /// Fill in the fields of a TypeGenericContextDescriptorHeader.
    void addGenericParametersHeader() {
      asImpl().addMetadataInstantiationCache();

      asImpl().addMetadataInstantiationPattern();

      super::addGenericParametersHeader();
    }

    void addMetadataInstantiationPattern() {
      if (!HasMetadata) {
        B.addInt32(0);
        return;
      }

      auto pattern = IGM.getAddrOfTypeMetadataPattern(Type);
      B.addRelativeAddress(pattern);
    }

    void addMetadataInstantiationCache() {
      if (!HasMetadata) {
        B.addInt32(0);
        return;
      }

      auto cache =
        IGM.getAddrOfTypeMetadataInstantiationCache(Type, NotForDefinition);
      B.addRelativeAddress(cache);
    }
      
    bool isUniqueDescriptor() {
      return !isa<ClangModuleUnit>(Type->getModuleScopeContext());
    }
    
    llvm::Constant *emit() {
      asImpl().layout();
      auto addr = IGM.getAddrOfTypeContextDescriptor(Type, HasMetadata,
                                                     B.finishAndCreateFuture());
      auto var = cast<llvm::GlobalVariable>(addr);
      
      var->setConstant(true);
      IGM.setTrueConstGlobal(var);
      return var;
    }
    
    /// Flags to indicate Clang-imported declarations so we mangle them
    /// consistently at runtime.
    void getClangImportedFlags(TypeContextDescriptorFlags &flags) const {
      auto clangDecl = Mangle::ASTMangler::getClangDeclForMangling(Type);
      if (!clangDecl)
        return;
      
      if (isa<clang::TagDecl>(clangDecl)) {
        flags.setIsCTag(true);
        return;
      }
      
      if (isa<clang::TypedefNameDecl>(clangDecl)
          || isa<clang::ObjCCompatibleAliasDecl>(clangDecl)) {
        flags.setIsCTypedef(true);
        return;
      }
      
      return;
    }

    // Subclasses should provide:
    // ContextDescriptorKind getContextKind();
    // void addLayoutInfo(); // ABI TODO: should be superseded
  };

  /// Build a doubly-null-terminated list of field names.
  ///
  /// ABI TODO: This should be unnecessary when the fields that use it are
  /// superseded.
  template<typename ValueDeclRange>
  unsigned getFieldNameString(const ValueDeclRange &fields,
                              llvm::SmallVectorImpl<char> &out) {
    unsigned numFields = 0;

    {
      llvm::raw_svector_ostream os(out);
      
      for (ValueDecl *prop : fields) {
        os << prop->getBaseName() << '\0';
        ++numFields;
      }
      // The final null terminator is provided by getAddrOfGlobalString.
    }
    return numFields;
  }
  
  /// Build the field type vector accessor for a nominal type. This is a
  /// function that lazily instantiates the type metadata for all of the
  /// types of the stored properties of an instance of a nominal type.
  ///
  /// ABI TODO: This should be unnecessary when the fields that use it are
  /// superseded.
  static void addFieldTypes(IRGenModule &IGM, ArrayRef<CanType> fieldTypes) {
    IGM.addFieldTypes(fieldTypes);
  }
  
  /// Build a field type accessor for stored properties.
  ///
  /// ABI TODO: This should be unnecessary when the fields that use it are
  /// superseded.
  static void
  addFieldTypes(IRGenModule &IGM, NominalTypeDecl *type,
                NominalTypeDecl::StoredPropertyRange storedProperties) {
    SmallVector<CanType, 4> types;
    for (VarDecl *prop : storedProperties) {
      auto propertyType = type->mapTypeIntoContext(prop->getInterfaceType())
                              ->getCanonicalType();
      types.push_back(propertyType);
    }

    addFieldTypes(IGM, types);
  }
  
  /// Build a case type accessor for enum payloads.
  ///
  /// ABI TODO: This should be unnecessary when the fields that use it are
  /// superseded.
  static void addFieldTypes(IRGenModule &IGM,
                            ArrayRef<EnumImplStrategy::Element> enumElements) {
    SmallVector<CanType, 4> types;

    for (auto &elt : enumElements) {
      auto caseType = elt.decl->getParentEnum()->mapTypeIntoContext(
        elt.decl->getArgumentInterfaceType())
          ->getCanonicalType();
      types.push_back(caseType);
    }

    addFieldTypes(IGM, types);
  }


  class StructContextDescriptorBuilder
    : public TypeContextDescriptorBuilderBase<StructContextDescriptorBuilder>
  {
    using super = TypeContextDescriptorBuilderBase;
  
    StructDecl *getType() {
      return cast<StructDecl>(Type);
    }

    Size FieldVectorOffset;

  public:
    StructContextDescriptorBuilder(IRGenModule &IGM, StructDecl *Type,
                                   RequireMetadata_t requireMetadata)
      : super(IGM, Type, requireMetadata)
    {
      auto &layout = IGM.getMetadataLayout(getType());
      FieldVectorOffset = layout.getFieldOffsetVectorOffset().getStatic();
    }

    ContextDescriptorKind getContextKind() {
      return ContextDescriptorKind::Struct;
    }
    
    void addLayoutInfo() {
      auto properties = getType()->getStoredProperties();

      // uint32_t NumFields;
      B.addInt32(std::distance(properties.begin(), properties.end()));

      // uint32_t FieldOffsetVectorOffset;
      B.addInt32(FieldVectorOffset / IGM.getPointerSize());

      addFieldTypes(IGM, getType(), properties);
    }
    
    uint16_t getKindSpecificFlags() {
      TypeContextDescriptorFlags flags;

      flags.setIsReflectable(true); // struct always reflectable

      getClangImportedFlags(flags);
      return flags.getOpaqueValue();
    }
  };
  
  class EnumContextDescriptorBuilder
    : public TypeContextDescriptorBuilderBase<EnumContextDescriptorBuilder>
  {
    using super = TypeContextDescriptorBuilderBase;
  
    EnumDecl *getType() {
      return cast<EnumDecl>(Type);
    }
    
    Size PayloadSizeOffset;
    const EnumImplStrategy &Strategy;
    
  public:
    EnumContextDescriptorBuilder(IRGenModule &IGM, EnumDecl *Type,
                                 RequireMetadata_t requireMetadata)
      : super(IGM, Type, requireMetadata),
        Strategy(getEnumImplStrategy(IGM,
                     getType()->getDeclaredTypeInContext()->getCanonicalType()))
    {
      auto &layout = IGM.getMetadataLayout(getType());
      if (layout.hasPayloadSizeOffset())
        PayloadSizeOffset = layout.getPayloadSizeOffset().getStatic();
    }
    
    ContextDescriptorKind getContextKind() {
      return ContextDescriptorKind::Enum;
    }
    
    void addLayoutInfo() {
      // # payload cases in the low 24 bits, payload size offset in the high 8.
      unsigned numPayloads = Strategy.getElementsWithPayload().size();
      assert(numPayloads < (1<<24) && "too many payload elements for runtime");
      assert(PayloadSizeOffset % IGM.getPointerAlignment() == Size(0)
             && "payload size not word-aligned");
      unsigned PayloadSizeOffsetInWords
        = PayloadSizeOffset / IGM.getPointerSize();
      assert(PayloadSizeOffsetInWords < 0x100 &&
             "payload size offset too far from address point for runtime");

      // uint32_t NumPayloadCasesAndPayloadSizeOffset;
      B.addInt32(numPayloads | (PayloadSizeOffsetInWords << 24));

      // uint32_t NumEmptyCases;
      B.addInt32(Strategy.getElementsWithNoPayload().size());

      addFieldTypes(IGM, Strategy.getElementsWithPayload());
    }
    
    uint16_t getKindSpecificFlags() {
      TypeContextDescriptorFlags flags;

      flags.setIsReflectable(Strategy.isReflectable());

      getClangImportedFlags(flags);
      return flags.getOpaqueValue();
    }
  };
  
  class ClassContextDescriptorBuilder
    : public TypeContextDescriptorBuilderBase<ClassContextDescriptorBuilder>,
      public SILVTableVisitor<ClassContextDescriptorBuilder>
  {
    using super = TypeContextDescriptorBuilderBase;
  
    ClassDecl *getType() {
      return cast<ClassDecl>(Type);
    }

    // Non-null unless the type is foreign.
    ClassMetadataLayout *MetadataLayout = nullptr;

    Optional<TypeEntityReference> SuperClassRef;

    SILVTable *VTable = nullptr;
    unsigned VTableSize = 0;

  public:
    ClassContextDescriptorBuilder(IRGenModule &IGM, ClassDecl *Type,
                                  RequireMetadata_t requireMetadata)
      : super(IGM, Type, requireMetadata)
    {
      if (getType()->isForeign()) return;

      MetadataLayout = &IGM.getClassMetadataLayout(Type);

      if (auto superclassDecl = getType()->getSuperclassDecl()) {
        SuperClassRef = IGM.getTypeEntityReference(superclassDecl);
      }

      VTableSize = MetadataLayout->getVTableSize();
      if (VTableSize) {
        VTable = IGM.getSILModule().lookUpVTable(getType());
      }
    }
    
    void layout() {
      super::layout();
      addVTable();
    }

    ContextDescriptorKind getContextKind() {
      return ContextDescriptorKind::Class;
    }
    
    uint16_t getKindSpecificFlags() {
      TypeContextDescriptorFlags flags;

      // Classes are always reflectable.
      flags.setIsReflectable(true);

      if (!getType()->isForeign()) {
        if (MetadataLayout->areImmediateMembersNegative())
          flags.class_setAreImmediateMembersNegative(true);

        if (VTableSize != 0)
          flags.class_setHasVTable(true);

        if (MetadataLayout->hasResilientSuperclass())
          flags.class_setHasResilientSuperclass(true);
      }

      if (SuperClassRef) {
        flags.class_setSuperclassReferenceKind(SuperClassRef->getKind());
      }
      
      getClangImportedFlags(flags);
      
      return flags.getOpaqueValue();
    }
    
    Size getFieldVectorOffset() {
      if (!MetadataLayout) return Size(0);
      return (MetadataLayout->hasResilientSuperclass()
                ? MetadataLayout->getRelativeFieldOffsetVectorOffset()
                : MetadataLayout->getStaticFieldOffsetVectorOffset());
    }
    
    void addVTable() {
      if (VTableSize == 0)
        return;

      auto offset = MetadataLayout->hasResilientSuperclass()
                      ? MetadataLayout->getRelativeVTableOffset()
                      : MetadataLayout->getStaticVTableOffset();
      B.addInt32(offset / IGM.getPointerSize());
      B.addInt32(VTableSize);
      
      addVTableEntries(getType());
    }
    
    void addMethod(SILDeclRef fn) {
      assert(VTable && "no vtable?!");

      auto descriptor = B.beginStruct(IGM.MethodDescriptorStructTy);

      // Classify the method.
      using Flags = MethodDescriptorFlags;
      auto flags = getMethodDescriptorFlags<Flags>(fn.getDecl());

      // Remember if the declaration was dynamic.
      if (fn.getDecl()->isDynamic())
        flags = flags.withIsDynamic(true);

      // TODO: final? open?

      auto *dc = fn.getDecl()->getDeclContext();
      assert(!isa<ExtensionDecl>(dc));

      if (fn.getDecl()->getDeclContext() == getType()) {
        if (auto entry = VTable->getEntry(IGM.getSILModule(), fn)) {
          assert(entry->TheKind == SILVTable::Entry::Kind::Normal);
          auto *implFn = IGM.getAddrOfSILFunction(entry->Implementation,
                                                  NotForDefinition);
          descriptor.addRelativeAddress(implFn);
        } else {
          // The method is removed by dead method elimination.
          // It should be never called. We add a pointer to an error function.
          descriptor.addRelativeAddressOrNull(nullptr);
        }
      }

      descriptor.addInt(IGM.Int32Ty, flags.getIntValue());

      descriptor.finishAndAddTo(B);
    }

    void addMethodOverride(SILDeclRef baseRef, SILDeclRef declRef) {}

    void addPlaceholder(MissingMemberDecl *MMD) {
      llvm_unreachable("cannot generate metadata with placeholders in it");
    }
    
    void addLayoutInfo() {
      auto properties = getType()->getStoredProperties();

      // RelativeDirectPointer<const void, /*nullable*/ true> SuperClass;
      if (SuperClassRef) {
        B.addRelativeAddress(SuperClassRef->getValue());
      } else {
        B.addInt32(0);
      }

      // union {
      //   uint32_t MetadataNegativeSizeInWords;
      //   RelativeDirectPointer<StoredClassMetadataBounds>
      //     ResilientMetadataBounds;
      // };
      if (!MetadataLayout) {
        // FIXME: do something meaningful for foreign classes?
        B.addInt32(0);
      } else if (!MetadataLayout->hasResilientSuperclass()) {
        B.addInt32(MetadataLayout->getSize().AddressPoint
                     / IGM.getPointerSize());
      } else {
        B.addRelativeAddress(
          IGM.getAddrOfClassMetadataBounds(getType(), NotForDefinition));
      }

      // union {
      //   uint32_t MetadataPositiveSizeInWords;
      // };
      if (!MetadataLayout) {
        // FIXME: do something meaningful for foreign classes?
        B.addInt32(0);
      } else if (!MetadataLayout->hasResilientSuperclass()) {
        B.addInt32(MetadataLayout->getSize().getOffsetToEnd()
                     / IGM.getPointerSize());
      } else {
        B.addInt32(0); // currently unused
      }

      // uint32_t NumImmediateMembers;
      auto numImmediateMembers =
        (MetadataLayout ? MetadataLayout->getNumImmediateMembers() : 0);
      B.addInt32(numImmediateMembers);

      // uint32_t NumFields;
      B.addInt32(std::distance(properties.begin(), properties.end()));

      // uint32_t FieldOffsetVectorOffset;
      B.addInt32(getFieldVectorOffset() / IGM.getPointerSize());

      addFieldTypes(IGM, getType(), properties);
    }
  };
} // end anonymous namespace

static void eraseExistingTypeContextDescriptor(IRGenModule &IGM,
                                               NominalTypeDecl *type) {
  // We may have emitted a partial type context descriptor with some empty
  // fields, and then later discovered we're emitting complete metadata.
  // Remove existing definitions of the type context so that we can regenerate
  // a complete descriptor.
  auto entity = IGM.getAddrOfTypeContextDescriptor(type, DontRequireMetadata);
  entity = entity->stripPointerCasts();
  auto existingContext = dyn_cast<llvm::GlobalVariable>(entity);
  if (existingContext && !existingContext->isDeclaration()) {
    existingContext->setInitializer(nullptr);
  }
}

void irgen::emitLazyTypeContextDescriptor(IRGenModule &IGM,
                                          NominalTypeDecl *type,
                                          RequireMetadata_t requireMetadata) {
  eraseExistingTypeContextDescriptor(IGM, type);

  if (auto sd = dyn_cast<StructDecl>(type)) {
    StructContextDescriptorBuilder(IGM, sd, requireMetadata).emit();
  } else if (auto ed = dyn_cast<EnumDecl>(type)) {
    EnumContextDescriptorBuilder(IGM, ed, requireMetadata).emit();
  } else if (auto cd = dyn_cast<ClassDecl>(type)) {
    ClassContextDescriptorBuilder(IGM, cd, requireMetadata).emit();
  } else {
    llvm_unreachable("type does not have a context descriptor");
  }
}

void irgen::emitLazyTypeMetadata(IRGenModule &IGM, NominalTypeDecl *type) {
  eraseExistingTypeContextDescriptor(IGM, type);

  if (auto sd = dyn_cast<StructDecl>(type)) {
    return emitStructMetadata(IGM, sd);
  } else if (auto ed = dyn_cast<EnumDecl>(type)) {
    emitEnumMetadata(IGM, ed);
  } else if (auto pd = dyn_cast<ProtocolDecl>(type)) {
    IGM.emitProtocolDecl(pd);
  } else {
    llvm_unreachable("should not have enqueued a class decl here!");
  }

}

llvm::Constant *
IRGenModule::getAddrOfSharedContextDescriptor(LinkEntity entity,
                                              ConstantInit definition,
                                              llvm::function_ref<void()> emit) {
  if (!definition) {
    // Generate the definition if it hasn't been generated yet.
    auto existing = GlobalVars.find(entity);
    if (existing == GlobalVars.end() ||
        !existing->second
        || cast<llvm::GlobalValue>(existing->second)->isDeclaration()) {
      
      // In some cases we have multiple declarations in the AST that end up
      // with the same context mangling (a clang module and its overlay,
      // equivalent extensions, etc.). These can share a context descriptor
      // at runtime.
      auto mangledName = entity.mangleAsString();
      if (auto otherDefinition = Module.getGlobalVariable(mangledName)) {
        GlobalVars.insert({entity, otherDefinition});
        return otherDefinition;
      }
      
      // Otherwise, emit the descriptor.
      emit();
    }
  }
  
  return getAddrOfLLVMVariable(entity, Alignment(4),
                               definition,
                               TypeContextDescriptorTy,
                               DebugTypeInfo());
}

llvm::Constant *
IRGenModule::getAddrOfModuleContextDescriptor(ModuleDecl *D,
                                              ConstantInit definition) {
  auto entity = LinkEntity::forModuleDescriptor(D);
  return getAddrOfSharedContextDescriptor(entity, definition,
    [&]{ ModuleContextDescriptorBuilder(*this, D).emit(); });
}

llvm::Constant *
IRGenModule::getAddrOfObjCModuleContextDescriptor() {
  if (!ObjCModule)
    ObjCModule = ModuleDecl::create(
      Context.getIdentifier(MANGLING_MODULE_OBJC),
      Context);
  return getAddrOfModuleContextDescriptor(ObjCModule);
}

llvm::Constant *
IRGenModule::getAddrOfClangImporterModuleContextDescriptor() {
  if (!ClangImporterModule)
    ClangImporterModule = ModuleDecl::create(
      Context.getIdentifier(MANGLING_MODULE_CLANG_IMPORTER),
      Context);
  return getAddrOfModuleContextDescriptor(ClangImporterModule);
}

llvm::Constant *
IRGenModule::getAddrOfExtensionContextDescriptor(ExtensionDecl *ED,
                                                 ConstantInit definition) {
  auto entity = LinkEntity::forExtensionDescriptor(ED);
  return getAddrOfSharedContextDescriptor(entity, definition,
    [&]{ ExtensionContextDescriptorBuilder(*this, ED).emit(); });
}

llvm::Constant *
IRGenModule::getAddrOfAnonymousContextDescriptor(DeclContext *DC,
                                                 ConstantInit definition) {
  auto entity = LinkEntity::forAnonymousDescriptor(DC);
  return getAddrOfSharedContextDescriptor(entity, definition,
    [&]{ AnonymousContextDescriptorBuilder(*this, DC).emit(); });
}

void IRGenModule::addFieldTypes(ArrayRef<CanType> fieldTypes) {
  IRGen.addFieldTypes(fieldTypes, this);
}

/*****************************************************************************/
/** Metadata Emission ********************************************************/
/*****************************************************************************/

namespace {
  /// An adapter class which turns a metadata layout class into a
  /// generic metadata layout class.
  template <class Impl, class Base>
  class GenericMetadataBuilderBase : public Base {
    using super = Base;

    struct FillOp {
      CanType Type;
      Optional<ProtocolConformanceRef> Conformance;
    };

  protected:
    using super::IGM;
    using super::asImpl;
    using super::Target;
    using super::B;

    /// Set to true if the metadata record for the generic type has fields
    /// outside of the generic parameter vector.
    bool HasDependentMetadata = false;
    
    /// Set to true if the value witness table for the generic type is dependent
    /// on its generic parameters. Implies HasDependentMetadata.
    bool HasDependentVWT = false;
    
    template <class... T>
    GenericMetadataBuilderBase(IRGenModule &IGM, T &&...args)
      : super(IGM, std::forward<T>(args)...) {}

    /// Emit the instantiation cache variable for the template.
    void emitInstantiationCache() {
      auto cache = cast<llvm::GlobalVariable>(
        IGM.getAddrOfTypeMetadataInstantiationCache(Target, ForDefinition));
      auto init =
        llvm::ConstantAggregateZero::get(cache->getValueType());
      cache->setInitializer(init);
    }

    /// Emit the create function for the template.
    void emitInstantiationFunction() {
      // using MetadataInstantiator =
      //   Metadata *(TypeContextDescriptor *type,
      //              const void * const *arguments,
      //              const GenericMetadataPattern *pattern);
      llvm::Function *f =
        IGM.getAddrOfTypeMetadataInstantiationFunction(Target, ForDefinition);
      f->setAttributes(IGM.constructInitialAttributes());

      IRGenFunction IGF(IGM, f);

      // Skip instrumentation when building for TSan to avoid false positives.
      // The synchronization for this happens in the Runtime and we do not see it.
      if (IGM.IRGen.Opts.Sanitizers & SanitizerKind::Thread)
        f->removeFnAttr(llvm::Attribute::SanitizeThread);

      if (IGM.DebugInfo)
        IGM.DebugInfo->emitArtificialFunction(IGF, f);

      Explosion params = IGF.collectParameters();
      llvm::Value *descriptor = params.claimNext();
      llvm::Value *args = params.claimNext();
      llvm::Value *templatePointer = params.claimNext();

      // Bind the generic arguments.
      if (Target->isGenericContext()) {
        Address argsArray(args, IGM.getPointerAlignment());
        emitPolymorphicParametersFromArray(IGF, Target, argsArray,
                                           MetadataState::Abstract);
      }

      // Allocate the metadata.
      llvm::Value *metadata =
        asImpl().emitAllocateMetadata(IGF, descriptor, args, templatePointer);

      IGF.Builder.CreateRet(metadata);
    }

    void emitCompletionFunction() {
      // using MetadataCompleter =
      //   MetadataDependency(Metadata *type,
      //                      MetadataCompletionContext *context,
      //                      const GenericMetadataPattern *pattern);
      llvm::Function *f =
        IGM.getAddrOfTypeMetadataCompletionFunction(Target, ForDefinition);
      f->setAttributes(IGM.constructInitialAttributes());

      IRGenFunction IGF(IGM, f);

      // Skip instrumentation when building for TSan to avoid false positives.
      // The synchronization for this happens in the Runtime and we do not see it.
      if (IGM.IRGen.Opts.Sanitizers & SanitizerKind::Thread)
        f->removeFnAttr(llvm::Attribute::SanitizeThread);

      if (IGM.DebugInfo)
        IGM.DebugInfo->emitArtificialFunction(IGF, f);

      Explosion params = IGF.collectParameters();
      llvm::Value *metadata = params.claimNext();
      llvm::Value *context = params.claimNext();
      llvm::Value *templatePointer = params.claimNext();

      (void) context;
      (void) templatePointer;

      // Bind the generic arguments.
      // FIXME: this will be problematic if we ever try to bind superclass
      // types from type metadata!
      if (Target->isGenericContext()) {
        auto type = Target->getDeclaredTypeInContext()->getCanonicalType();
        IGF.bindLocalTypeDataFromTypeMetadata(type, IsExact, metadata,
                                              MetadataState::Abstract);
      }

      // A dependent VWT means that we have dependent metadata.
      if (HasDependentVWT)
        HasDependentMetadata = true;

      MetadataDependencyCollector collector;

      if (HasDependentMetadata) {
        asImpl().emitInitializeMetadata(IGF, metadata, false, &collector);
      }
      
      // The metadata is now complete.  Finalize any metadata dependencies
      // we may have collected.
      auto dependency = collector.finish(IGF);
      auto returnValue = dependency.combine(IGF);

      IGF.Builder.CreateRet(returnValue);
    }

    /// The information necessary to fill in a GenericMetadataPartialPattern
    /// structure.
    struct PartialPattern {
      llvm::Constant *Data;
      Size DataOffset;
      Size DataSize;
    };
    void addPartialPattern(PartialPattern pattern) {
      // RelativeDirectPointer<void*> Pattern;
      B.addRelativeAddress(pattern.Data);

      // uint16_t OffsetInWords;
      B.addInt16(IGM.getOffsetInWords(pattern.DataOffset));

      // uint16_t SizeInWords;
      B.addInt16(IGM.getOffsetInWords(pattern.DataSize));
    }

  public:
    void createMetadataAccessFunction() {
      (void) getGenericTypeMetadataAccessFunction(IGM, Target, ForDefinition);
    }

    void layout() {
      asImpl().layoutHeader();

      if (asImpl().hasExtraDataPattern()) {
        asImpl().addExtraDataPattern();
      }

      // Immediate-members pattern.  This is only valid for classes.
      if (asImpl().hasImmediateMembersPattern()) {
        asImpl().addImmediateMembersPattern();
      }

      // We're done with the pattern now.
#ifndef NDEBUG
      auto finalOffset = B.getNextOffsetFromGlobal();
#endif

      asImpl().emitInstantiationDefinitions();

      assert(finalOffset == B.getNextOffsetFromGlobal() &&
             "emitInstantiationDefinitions added members to the pattern!");
    }

    // Emit the fields of GenericMetadataPattern.
    void layoutHeader() {
      // RelativePointer<MetadataInstantiator> InstantiationFunction;
      asImpl().addInstantiationFunction();

      // RelativePointer<MetadataCompleter> CompletionFunction;
      asImpl().addCompletionFunction();

      // ClassMetadataPatternFlags PatternFlags;
      asImpl().addPatternFlags();
    }

    void addInstantiationFunction() {
      auto function = IGM.getAddrOfTypeMetadataInstantiationFunction(Target,
                                                              NotForDefinition);
      B.addRelativeAddress(function);
    }

    void addCompletionFunction() {
      if (!asImpl().hasCompletionFunction()) {
        B.addInt32(0);
        return;
      }

      auto function = IGM.getAddrOfTypeMetadataCompletionFunction(Target,
                                                              NotForDefinition);
      B.addRelativeAddress(function);
    }

    void addPatternFlags() {
      GenericMetadataPatternFlags flags = asImpl().getPatternFlags();
      B.addInt32(flags.getOpaqueValue());
    }

    GenericMetadataPatternFlags getPatternFlags() {
      GenericMetadataPatternFlags flags;

      if (asImpl().hasExtraDataPattern())
        flags.setHasExtraDataPattern(true);

      return flags;
    }

    bool hasExtraDataPattern() {
      return false;
    }
    void addExtraDataPattern() {
      asImpl().addPartialPattern(asImpl().buildExtraDataPattern());
    }
    PartialPattern buildExtraDataPattern() {
      llvm_unreachable("no extra data pattern!");
    }

    bool hasImmediateMembersPattern() {
      return false;
    }
    void addImmediateMembersPattern() {
      asImpl().addPartialPattern(asImpl().buildImmediateMembersPattern());
    }
    PartialPattern buildImmediateMembersPattern() {
      llvm_unreachable("no immediate members pattern!");
    }

    void emitInstantiationDefinitions() {
      // Force the emission of the nominal type descriptor, although we
      // don't use it yet.
      (void) asImpl().emitNominalTypeDescriptor();

      // Emit the instantiation function.
      asImpl().emitInstantiationFunction();

      // Emit the completion function.
      if (asImpl().hasCompletionFunction())
        asImpl().emitCompletionFunction();

      // Emit the instantiation cache.
      asImpl().emitInstantiationCache();
    }
  };

  template <class Impl, class Base>
  class GenericValueMetadataBuilderBase
         : public GenericMetadataBuilderBase<Impl, Base> {
    using super = GenericMetadataBuilderBase<Impl, Base>;
  protected:
    using super::IGM;
    using super::asImpl;
    using super::Target;
    using super::B;

    template <class... T>
    GenericValueMetadataBuilderBase(IRGenModule &IGM, T &&...args)
      : super(IGM, std::forward<T>(args)...) {}

  public:
    /// Emit the fields of a GenericValueMetadataPattern.
    void layoutHeader() {
      super::layoutHeader();

      // RelativeIndirectablePointer<const ValueWitnessTable> ValueWitnesses;
      asImpl().addValueWitnessTable();

    }

    GenericMetadataPatternFlags getPatternFlags() {
      auto flags = super::getPatternFlags();

      flags.value_setMetadataKind(asImpl().getMetadataKind());

      assert(!asImpl().hasImmediateMembersPattern());

      return flags;
    }

    void addValueWitnessTable() {
      auto table = asImpl().emitValueWitnessTable();
      B.addRelativeAddress(table);
    }
  };
} // end anonymous namespace

static void emitInitializeFieldOffsetVector(IRGenFunction &IGF,
                                            SILType T,
                                            llvm::Value *metadata,
                                            bool isVWTMutable,
                                       MetadataDependencyCollector *collector) {
  auto *target = T.getNominalOrBoundGenericNominal();
  llvm::Value *fieldVector
    = emitAddressOfFieldOffsetVector(IGF, metadata, target)
      .getAddress();
  
  // Collect the stored properties of the type.
  llvm::SmallVector<VarDecl*, 4> storedProperties;
  for (auto prop : target->getStoredProperties()) {
    storedProperties.push_back(prop);
  }

  // Fill out an array with the field type metadata records.
  Address fields = IGF.createAlloca(
                   llvm::ArrayType::get(IGF.IGM.Int8PtrPtrTy,
                                        storedProperties.size()),
                   IGF.IGM.getPointerAlignment(), "classFields");
  IGF.Builder.CreateLifetimeStart(fields,
                  IGF.IGM.getPointerSize() * storedProperties.size());
  fields = IGF.Builder.CreateStructGEP(fields, 0, Size(0));

  unsigned index = 0;
  for (auto prop : storedProperties) {
    auto propTy = T.getFieldType(prop, IGF.getSILModule());
    llvm::Value *metadata = emitTypeLayoutRef(IGF, propTy, collector);
    Address field = IGF.Builder.CreateConstArrayGEP(fields, index,
                                                    IGF.IGM.getPointerSize());
    IGF.Builder.CreateStore(metadata, field);
    ++index;
  }

  // Ask the runtime to lay out the class.  This can relocate it if it
  // wasn't allocated with swift_allocateGenericClassMetadata.
  auto numFields = IGF.IGM.getSize(Size(storedProperties.size()));

  if (isa<ClassDecl>(target)) {
    ClassLayoutFlags flags = ClassLayoutFlags::Swift5Algorithm;

    IGF.Builder.CreateCall(IGF.IGM.getInitClassMetadataFn(),
                           {metadata, IGF.IGM.getSize(Size(uintptr_t(flags))),
                            numFields, fields.getAddress(), fieldVector});
  } else {
    assert(isa<StructDecl>(target));
    StructLayoutFlags flags = StructLayoutFlags::Swift5Algorithm;
    if (isVWTMutable)
      flags |= StructLayoutFlags::IsVWTMutable;

    IGF.Builder.CreateCall(IGF.IGM.getInitStructMetadataFn(),
                           {metadata, IGF.IGM.getSize(Size(uintptr_t(flags))),
                            numFields, fields.getAddress(), fieldVector});
  }

  IGF.Builder.CreateLifetimeEnd(fields,
                  IGF.IGM.getPointerSize() * storedProperties.size());
}

// Classes

namespace {
  /// Utility class for building member metadata for classes where the
  /// entire hierarchy is in the current resilience domain, and all stored
  /// properties have a fixed size.
  class FixedClassMemberBuilder {
    IRGenModule &IGM;
    ConstantStructBuilder &B;
    const StructLayout &Layout;
    const ClassLayout &FieldLayout;
    SILVTable *VTable;

  public:
    FixedClassMemberBuilder(IRGenModule &IGM, ClassDecl *theClass,
                            ConstantStructBuilder &builder,
                            const StructLayout &layout,
                            const ClassLayout &fieldLayout)
      : IGM(IGM), B(builder), Layout(layout), FieldLayout(fieldLayout) {
      VTable = IGM.getSILModule().lookUpVTable(theClass);
    }

    void addFieldOffset(VarDecl *var) {
      unsigned fieldIndex = FieldLayout.getFieldIndex(var);
      auto &element = Layout.getElement(fieldIndex);
      assert(element.getKind() == ElementLayout::Kind::Fixed ||
             element.getKind() == ElementLayout::Kind::Empty);

      B.addInt(IGM.SizeTy, element.getByteOffset().getValue());
    }

    void addFieldOffsetPlaceholders(MissingMemberDecl *placeholder) {
      for (unsigned i = 0,
                    e = placeholder->getNumberOfFieldOffsetVectorEntries();
           i < e; ++i) {
        // Emit placeholder values for some number of stored properties we
        // know exist but aren't able to reference directly.
        B.addInt(IGM.SizeTy, 0);
      }
    }

    void addMethod(SILDeclRef fn) {
      // Find the vtable entry.
      assert(VTable && "no vtable?!");
      auto entry = VTable->getEntry(IGM.getSILModule(), fn);

      // The class is fragile. Emit a direct reference to the vtable entry.
      if (entry) {
        B.add(IGM.getAddrOfSILFunction(entry->Implementation, NotForDefinition));
        return;
      }

      // The method is removed by dead method elimination.
      // It should be never called. We add a pointer to an error function.
      B.addBitCast(IGM.getDeletedMethodErrorFn(), IGM.FunctionPtrTy);
    }

    void emitInitializeMethodOverrides(IRGenFunction &IGF,
                                       llvm::Value *metadata) {}

    void addGenericArgument(CanType argTy, ClassDecl *forClass) {
      B.addNullPointer(IGM.TypeMetadataPtrTy);
    }

    void addGenericWitnessTable(CanType argTy, ProtocolConformanceRef conf,
                                ClassDecl *forClass) {
      B.addNullPointer(IGM.WitnessTablePtrTy);
    }
  };

  /// Utility class for building member metadata for classes that inherit
  /// from a class in a different resilience domain, or have fields whose
  /// size is not known at compile time.
  class ResilientClassMemberBuilder {
    IRGenModule &IGM;
    SILVTable *VTable;

  public:
    ResilientClassMemberBuilder(IRGenModule &IGM, ClassDecl *theClass,
                                ConstantStructBuilder &builder,
                                const StructLayout &layout,
                                const ClassLayout &fieldLayout)
        : IGM(IGM) {
      VTable = IGM.getSILModule().lookUpVTable(theClass);
    }

    void addFieldOffset(VarDecl *var) {}

    void addFieldOffsetPlaceholders(MissingMemberDecl *placeholder) {}

    void addMethod(SILDeclRef fn) {}

    // Update vtable entries for method overrides. The runtime copies in
    // the vtable from the superclass for us; we have to install method
    // overrides ourselves.
    void emitInitializeMethodOverrides(IRGenFunction &IGF,
                                       llvm::Value *metadata) {
      for (auto &entry : VTable->getEntries()) {
        if (entry.TheKind != SILVTable::Entry::Kind::Override)
          continue;

        auto fn = entry.Method;

        auto *classDecl = cast<ClassDecl>(fn.getDecl()->getDeclContext());
        auto &layout = IGM.getClassMetadataLayout(classDecl);

        auto offset = layout.getMethodInfo(IGF, fn).getOffset();

        auto slot = IGF.emitAddressAtOffset(metadata, offset,
                                            IGM.Int8PtrTy,
                                            IGM.getPointerAlignment());

        auto *implFn = IGM.getAddrOfSILFunction(entry.Implementation,
                                                NotForDefinition);
        auto *value = IGF.Builder.CreateBitCast(implFn, IGM.Int8PtrTy);
        IGF.Builder.CreateStore(value, slot);
      }
    }

    void addGenericArgument(CanType argTy, ClassDecl *forClass) {}

    void addGenericWitnessTable(CanType argTy, ProtocolConformanceRef conf,
                                ClassDecl *forClass) {}
  };

  /// Base class for laying out class metadata.
  template <class Impl, class MemberBuilder>
  class ClassMetadataBuilderBase : public ClassMetadataVisitor<Impl> {
    using super = ClassMetadataVisitor<Impl>;

  protected:
    using super::IGM;
    using super::Target;
    using super::asImpl;

    ConstantStructBuilder &B;
    const StructLayout &Layout;
    const ClassLayout &FieldLayout;
    ClassMetadataLayout &MetadataLayout;

    MemberBuilder Members;

    ClassMetadataBuilderBase(IRGenModule &IGM, ClassDecl *theClass,
                             ConstantStructBuilder &builder,
                             const StructLayout &layout,
                             const ClassLayout &fieldLayout)
      : super(IGM, theClass), B(builder),
        Layout(layout), FieldLayout(fieldLayout),
        MetadataLayout(IGM.getClassMetadataLayout(theClass)),
        Members(IGM, theClass, builder, layout, fieldLayout) {}

  public:
    void noteResilientSuperclass() {}

    void noteStartOfImmediateMembers(ClassDecl *theClass) {
      if (theClass == Target) {
        emitClassMetadataBaseOffset();
      }
    }

    /// Emit the base-offset variable for the class.
    void emitClassMetadataBaseOffset() {
      // Only classes defined in resilient modules, or those that have
      // a resilient superclass need this.
      if (!MetadataLayout.hasResilientSuperclass() &&
          !IGM.isResilient(Target, ResilienceExpansion::Minimal)) {
        return;
      }

      auto *offsetAddr =
        IGM.getAddrOfClassMetadataBounds(Target, ForDefinition);
      auto *offsetVar = cast<llvm::GlobalVariable>(offsetAddr);

      if (MetadataLayout.hasResilientSuperclass()) {
        // If the superclass is resilient to us, we have to compute and
        // initialize the global when we initialize the metadata.
        auto init = llvm::ConstantAggregateZero::get(offsetVar->getValueType());

        offsetVar->setInitializer(init);
        offsetVar->setConstant(false);
        return;
      }

      // Otherwise, we know the offset at compile time, even if our
      // clients do not, so just emit a constant.
      auto &layout = IGM.getClassMetadataLayout(Target);

      auto immediateMembersOffset = layout.getStartOfImmediateMembers();
      auto size = layout.getSize();
      auto negativeSizeInWords = size.AddressPoint / IGM.getPointerSize();
      auto positiveSizeInWords = size.getOffsetToEnd() / IGM.getPointerSize();

      auto initTy = cast<llvm::StructType>(offsetVar->getValueType());
      auto *init = llvm::ConstantStruct::get(initTy, {
        llvm::ConstantInt::get(IGM.SizeTy, immediateMembersOffset.getValue()),
        llvm::ConstantInt::get(IGM.Int32Ty, negativeSizeInWords),
        llvm::ConstantInt::get(IGM.Int32Ty, positiveSizeInWords)
      });

      offsetVar->setInitializer(init);
      offsetVar->setConstant(true);
    }

    /// The 'metadata flags' field in a class is actually a pointer to
    /// the metaclass object for the class.
    ///
    /// NONAPPLE: This is only really required for ObjC interop; maybe
    /// suppress this for classes that don't need to be exposed to
    /// ObjC, e.g. for non-Apple platforms?
    void addMetadataFlags() {
      static_assert(unsigned(MetadataKind::Class) == 0,
                    "class metadata kind is non-zero?");

      if (IGM.ObjCInterop) {
        // Get the metaclass pointer as an intptr_t.
        auto metaclass = IGM.getAddrOfMetaclassObject(Target,
                                                      NotForDefinition);
        auto flags =
          llvm::ConstantExpr::getPtrToInt(metaclass, IGM.MetadataKindTy);
        B.add(flags);
      } else {
        // On non-objc platforms just fill it with a null, there
        // is no Objective-C metaclass.
        // FIXME: Remove this to save metadata space.
        // rdar://problem/18801263
        B.addInt(IGM.MetadataKindTy, unsigned(MetadataKind::Class));
      }
    }

    /// The runtime provides a value witness table for Builtin.NativeObject.
    void addValueWitnessTable() {
      ClassDecl *cls = Target;
      
      auto type = (cls->checkObjCAncestry() != ObjCClassKind::NonObjC
                   ? IGM.Context.TheUnknownObjectType
                   : IGM.Context.TheNativeObjectType);
      auto wtable = IGM.getAddrOfValueWitnessTable(type);
      B.add(wtable);
    }

    void addDestructorFunction() {
      if (auto ptr = getAddrOfDestructorFunction()) {
        B.add(*ptr);
      } else {
        // In case the optimizer removed the function. See comment in
        // addMethod().
        B.addNullPointer(IGM.FunctionPtrTy);
      }
    }

    Optional<llvm::Constant *> getAddrOfDestructorFunction() {
      auto dtorRef = SILDeclRef(Target->getDestructor(),
                                SILDeclRef::Kind::Deallocator);
      SILFunction *dtorFunc = IGM.getSILModule().lookUpFunction(dtorRef);
      if (!dtorFunc) return llvm::None;
      return IGM.getAddrOfSILFunction(dtorFunc, NotForDefinition);
    }

    void addNominalTypeDescriptor() {
      auto descriptor = asImpl().emitNominalTypeDescriptor();
      B.add(descriptor);
    }

    llvm::Constant *emitNominalTypeDescriptor() {
      return ClassContextDescriptorBuilder(IGM, Target, RequireMetadata).emit();
    }

    void addIVarDestroyer() {
      auto dtorFunc = getAddrOfIVarDestroyer();
      if (dtorFunc) {
        B.add(*dtorFunc);
      } else {
        B.addNullPointer(IGM.FunctionPtrTy);
      }
    }

    Optional<llvm::Function *> getAddrOfIVarDestroyer() {
      return IGM.getAddrOfIVarInitDestroy(Target,
                                          /*isDestroyer=*/ true,
                                          /*isForeign=*/ false,
                                          NotForDefinition);
    }

    bool addReferenceToHeapMetadata(CanType type, bool allowUninitialized) {
      if (llvm::Constant *metadata
            = tryEmitConstantHeapMetadataRef(IGM, type, allowUninitialized)) {
        B.add(metadata);
        return true;
      } else {
        // Leave a null pointer placeholder to be filled at runtime
        B.addNullPointer(IGM.TypeMetadataPtrTy);
        return false;
      }
    }

    void addClassFlags() {
      auto flags = ClassFlags();

#if !SWIFT_DARWIN_ENABLE_STABLE_ABI_BIT
      // FIXME: Remove this after enabling stable ABI.
      // This bit is NOT conditioned on UseDarwinPreStableABIBit.
      flags |= ClassFlags::IsSwiftPreStableABI;
#endif

      // Set a flag if the class uses Swift refcounting.
      auto type = Target->getDeclaredType()->getCanonicalType();
      if (getReferenceCountingForType(IGM, type)
            == ReferenceCounting::Native) {
        flags |= ClassFlags::UsesSwiftRefcounting;
      }

      // Set a flag if the class has a custom ObjC name.
      DeclAttributes attrs = Target->getAttrs();
      if (auto objc = attrs.getAttribute<ObjCAttr>()) {
        if (objc->getName())
          flags |= ClassFlags::HasCustomObjCName;
      }
      if (attrs.hasAttribute<ObjCRuntimeNameAttr>())
        flags |= ClassFlags::HasCustomObjCName;

      B.addInt32((uint32_t) flags);
    }

    void addInstanceAddressPoint() {
      // Right now, we never allocate fields before the address point.
      B.addInt32(0);
    }

    void addInstanceSize() {
      if (llvm::Constant *size
            = tryEmitClassConstantFragileInstanceSize(IGM, Target)) {
        // We only support a maximum 32-bit instance size.
        if (IGM.SizeTy != IGM.Int32Ty)
          size = llvm::ConstantExpr::getTrunc(size, IGM.Int32Ty);
        B.add(size);
      } else {
        // Leave a zero placeholder to be filled at runtime
        B.addInt32(0);
      }
    }
    
    void addInstanceAlignMask() {
      if (llvm::Constant *align
            = tryEmitClassConstantFragileInstanceAlignMask(IGM, Target)) {
        if (IGM.SizeTy != IGM.Int16Ty)
          align = llvm::ConstantExpr::getTrunc(align, IGM.Int16Ty);
        B.add(align);
      } else {
        // Leave a zero placeholder to be filled at runtime
        B.addInt16(0);
      }
    }

    void addRuntimeReservedBits() {
      B.addInt16(0);
    }

    void addClassSize() {
      auto size = MetadataLayout.getSize();
      B.addInt32(size.FullSize.getValue());
    }

    void addClassAddressPoint() {
      // FIXME: Wrong
      auto size = MetadataLayout.getSize();
      B.addInt32(size.AddressPoint.getValue());
    }

    void addClassCacheData() {
      // We initially fill in these fields with addresses taken from
      // the ObjC runtime.
      // FIXME: Remove null data altogether rdar://problem/18801263
      B.add(IGM.getObjCEmptyCachePtr());
      B.add(IGM.getObjCEmptyVTablePtr());
    }

    void addClassDataPointer() {
      if (!IGM.ObjCInterop) {
        // with no Objective-C runtime, just give an empty pointer with the
        // swift bit set.
        // FIXME: Remove null data altogether rdar://problem/18801263
        B.addInt(IGM.IntPtrTy, 1);
        return;
      }

      // Derive the RO-data.
      llvm::Constant *data = emitClassPrivateData(IGM, Target);

      // Set a low bit to indicate this class has Swift metadata.
      auto bit = llvm::ConstantInt::get(IGM.IntPtrTy,
                                        IGM.UseDarwinPreStableABIBit ? 1 : 2);

      // Emit data + bit.
      data = llvm::ConstantExpr::getPtrToInt(data, IGM.IntPtrTy);
      data = llvm::ConstantExpr::getAdd(data, bit);
      B.add(data);
    }

    void addFieldOffset(VarDecl *var) {
      Members.addFieldOffset(var);
    }
    
    void addFieldOffsetPlaceholders(MissingMemberDecl *placeholder) {
      Members.addFieldOffsetPlaceholders(placeholder);
    }

    void addMethod(SILDeclRef fn) {
      Members.addMethod(fn);
    }

    void addPlaceholder(MissingMemberDecl *m) {
      assert(m->getNumberOfVTableEntries() == 0
             && "cannot generate metadata with placeholders in it");
    }

    void addMethodOverride(SILDeclRef baseRef, SILDeclRef declRef) {}

    void addGenericArgument(CanType argTy, ClassDecl *forClass) {
      Members.addGenericArgument(argTy, forClass);
    }

    void addGenericWitnessTable(CanType argTy, ProtocolConformanceRef conf,
                                ClassDecl *forClass) {
      Members.addGenericWitnessTable(argTy, conf, forClass);
    }

  protected:
    llvm::Value *emitFinishIdempotentInitialization(IRGenFunction &IGF,
                                                    llvm::Value *metadata) {
      if (IGF.IGM.ObjCInterop) {
        metadata =
          IGF.Builder.CreateBitCast(metadata, IGF.IGM.ObjCClassPtrTy);
        metadata =
          IGF.Builder.CreateCall(IGF.IGM.getGetInitializedObjCClassFn(),
                                 metadata);
        metadata =
           IGF.Builder.CreateBitCast(metadata, IGF.IGM.TypeMetadataPtrTy);
      }
      return metadata;
    }

    llvm::Value *emitFinishInitializationOfClassMetadata(IRGenFunction &IGF,
                                                         llvm::Value *metadata,
                                       MetadataDependencyCollector *collector) {
      if (doesClassMetadataRequireDynamicInitialization(IGF.IGM, Target)) {
        // We need to:
        //   - fill out the subclass's field offset vector
        //   - copy field offsets and generic arguments from higher in the
        //     class hierarchy
        auto classTy = Target->getDeclaredTypeInContext()->getCanonicalType();
        auto loweredClassTy = IGF.IGM.getLoweredType(classTy);
        emitInitializeFieldOffsetVector(IGF, loweredClassTy,
                                        metadata, /*VWT is mutable*/ false,
                                        collector);

        // Realizing the class with the ObjC runtime will copy back to the
        // field offset globals for us; but if ObjC interop is disabled, we
        // have to do that ourselves, assuming we didn't just emit them all
        // correctly in the first place.
        if (!IGF.IGM.ObjCInterop)
          emitInitializeFieldOffsets(IGF, metadata);
      } else {
        // Otherwise, all we need to do is register with the ObjC runtime.
        metadata = emitFinishIdempotentInitialization(IGF, metadata);
      }

      emitFieldOffsetGlobals();

      emitInitializeMethodOverrides(IGF, metadata);

      return metadata;
    }

    /// Materialize type metadata for the given type and store it into the
    /// superclass field of the given metadata.
    void emitStoreOfSuperclass(IRGenFunction &IGF, CanType superclassType,
                               llvm::Value *metadata,
                               MetadataDependencyCollector *collector) {
      auto request = DynamicMetadataRequest::getNonBlocking(
                               MetadataState::NonTransitiveComplete, collector);

      llvm::Value *superMetadata =
        emitClassHeapMetadataRef(IGF, superclassType,
                                 MetadataValueType::TypeMetadata,
                                 request,
                                 /*allowUninit*/ false);

      Address superField =
        emitAddressOfSuperclassRefInClassMetadata(IGF, metadata);
      superField = IGF.Builder.CreateElementBitCast(superField,
                                                    IGM.TypeMetadataPtrTy);
      IGF.Builder.CreateStore(superMetadata, superField);
    }

    // Update vtable entries for method overrides. The runtime copies in
    // the vtable from the superclass for us; we have to install method
    // overrides ourselves.
    void emitInitializeMethodOverrides(IRGenFunction &IGF,
                                       llvm::Value *metadata) {
      Members.emitInitializeMethodOverrides(IGF, metadata);
    }

    // The Objective-C runtime will copy field offsets from the field offset
    // vector into field offset globals for us, if present. If there's no
    // Objective-C runtime, we have to do this ourselves.
    void emitInitializeFieldOffsets(IRGenFunction &IGF,
                                    llvm::Value *metadata) {
      for (auto prop : Target->getStoredProperties()) {
        unsigned fieldIndex = FieldLayout.getFieldIndex(prop);
        auto access = FieldLayout.AllFieldAccesses[fieldIndex];
        if (access == FieldAccess::NonConstantDirect) {
          Address offsetA = IGF.IGM.getAddrOfFieldOffset(prop, ForDefinition);

          // We can't use emitClassFieldOffset() here because that creates
          // an invariant load, which could be hoisted above the point
          // where the metadata becomes fully initialized
          auto slot =
            emitAddressOfClassFieldOffset(IGF, metadata, Target, prop);
          auto offsetVal = IGF.emitInvariantLoad(slot);
          IGF.Builder.CreateStore(offsetVal, offsetA);
        }
      }
    }

    void emitFieldOffsetGlobals() {
      for (auto prop : Target->getStoredProperties()) {
        unsigned fieldIndex = FieldLayout.getFieldIndex(prop);
        llvm::Constant *fieldOffsetOrZero;
        auto &element = Layout.getElement(fieldIndex);

        if (element.getKind() == ElementLayout::Kind::Fixed) {
          // Use a fixed offset if we have one.
          fieldOffsetOrZero = IGM.getSize(element.getByteOffset());
        } else {
          // Otherwise, leave a placeholder for the runtime to populate at runtime.
          fieldOffsetOrZero = IGM.getSize(Size(0));
        }

        auto access = FieldLayout.AllFieldAccesses[fieldIndex];
        switch (access) {
        case FieldAccess::ConstantDirect:
        case FieldAccess::NonConstantDirect: {
          // Emit a global variable storing the constant field offset.
          // If the superclass was imported from Objective-C, the offset
          // does not include the superclass size; we rely on the
          // Objective-C runtime sliding it down.
          //
          // TODO: Don't emit the symbol if field has a fixed offset and size
          // in all resilience domains
          auto offsetAddr = IGM.getAddrOfFieldOffset(prop, ForDefinition);
          auto offsetVar = cast<llvm::GlobalVariable>(offsetAddr.getAddress());
          offsetVar->setInitializer(fieldOffsetOrZero);

          // If we know the offset won't change, make it a constant.
          offsetVar->setConstant(access == FieldAccess::ConstantDirect);

          break;
        }

        case FieldAccess::ConstantIndirect:
          // No global variable is needed.
          break;
        }
      }
    }
  };

  /// Base class for layout of non-generic class metadata.
  template<class Impl, class MemberBuilder>
  class ConcreteClassMetadataBuilderBase :
      public ClassMetadataBuilderBase<Impl, MemberBuilder> {

    using super = ClassMetadataBuilderBase<Impl, MemberBuilder>;

    using super::IGM;
    using super::Target;
    using super::B;
    using super::addReferenceToHeapMetadata;
    using super::emitFinishInitializationOfClassMetadata;
    using super::emitFinishIdempotentInitialization;
    using super::emitFieldOffsetGlobals;

    bool HasUnfilledSuperclass = false;
    Size AddressPoint;

  public:
    ConcreteClassMetadataBuilderBase(IRGenModule &IGM, ClassDecl *theClass,
                                     ConstantStructBuilder &builder,
                                     const StructLayout &layout,
                                     const ClassLayout &fieldLayout)
      : super(IGM, theClass, builder, layout, fieldLayout) {
    }

    void noteAddressPoint() {
      super::noteAddressPoint();
      AddressPoint = B.getNextOffsetFromGlobal();
    }

    void addSuperClass() {
      // If this is a root class, use SwiftObject as our formal parent.
      if (!Target->hasSuperclass()) {
        // This is only required for ObjC interoperation.
        if (!IGM.ObjCInterop) {
          B.addNullPointer(IGM.TypeMetadataPtrTy);
          return;
        }

        // We have to do getAddrOfObjCClass ourselves here because
        // the ObjC runtime base needs to be ObjC-mangled but isn't
        // actually imported from a clang module.
        B.add(IGM.getAddrOfObjCClass(
                               IGM.getObjCRuntimeBaseForSwiftRootClass(Target),
                               NotForDefinition));
        return;
      }

      Type superclassTy = Target->mapTypeIntoContext(Target->getSuperclass());

      if (!addReferenceToHeapMetadata(superclassTy->getCanonicalType(),
                                      /*allowUninit*/ false)) {
        HasUnfilledSuperclass = true;
      }
    }

    bool canBeConstant() {
      // TODO: the metadata global can actually be constant in a very
      // special case: it's not a pattern, ObjC interoperation isn't
      // required, there are no class fields, and there is nothing that
      // needs to be runtime-adjusted.
      return false;
    }

    void createMetadataAccessFunction() {
      assert(!Target->isGenericContext());
      auto type =cast<ClassType>(Target->getDeclaredType()->getCanonicalType());

      (void) getTypeMetadataAccessFunction(IGM, type, ForDefinition,
      [&](IRGenFunction &IGF, DynamicMetadataRequest request,
          llvm::Constant *cacheVar) -> MetadataResponse {
        // There's an interesting special case where we can do the
        // initialization idempotently and thus avoid the need for a lock.
        if (!HasUnfilledSuperclass &&
            !doesClassMetadataRequireDynamicInitialization(IGM, Target)) {
          emitFieldOffsetGlobals();

          auto type = Target->getDeclaredType()->getCanonicalType();
          auto metadata = IGF.IGM.getAddrOfTypeMetadata(type);
          return MetadataResponse::forComplete(
                   emitFinishIdempotentInitialization(IGF, metadata));
        }

        // Otherwise, use the generic path.
        return emitInPlaceTypeMetadataAccessFunctionBody(IGF, type, cacheVar,
          [&](IRGenFunction &IGF, llvm::Value *metadata) {
            return emitInPlaceMetadataInitialization(IGF, type, metadata);
          });
      });
    }

  private:
    llvm::Value *emitInPlaceMetadataInitialization(IRGenFunction &IGF,
                                                   CanClassType type,
                                                   llvm::Value *metadata) {
      // Many of the things done by generic instantiation are unnecessary here:
      //   initializing the metaclass pointer
      //   initializing the ro-data pointer

      MetadataDependencyCollector *collector = nullptr;

      // Initialize the superclass if we didn't do so as a constant.
      if (HasUnfilledSuperclass) {
        auto superclass = type->getSuperclass()->getCanonicalType();
        this->emitStoreOfSuperclass(IGF, superclass, metadata, collector);
      }

      // Relocate the metadata if it has a superclass that is resilient
      // to us.
      if (doesClassMetadataRequireDynamicInitialization(IGM, Target)) {
        auto templateSize = IGM.getSize(Size(B.getNextOffsetFromGlobal()));
        auto numImmediateMembers = IGM.getSize(
          Size(IGM.getClassMetadataLayout(Target).getNumImmediateMembers()));
        metadata = IGF.Builder.CreateCall(IGF.IGM.getRelocateClassMetadataFn(),
                                          {metadata, templateSize,
                                           numImmediateMembers});
      }

      return emitFinishInitializationOfClassMetadata(IGF, metadata, collector);
    }
  };

  /// A builder for fixed-size, non-generic class metadata.
  class FixedClassMetadataBuilder :
      public ConcreteClassMetadataBuilderBase<FixedClassMetadataBuilder,
                                              FixedClassMemberBuilder> {
    using super = ConcreteClassMetadataBuilderBase<FixedClassMetadataBuilder,
                                                   FixedClassMemberBuilder>;

  public:
    FixedClassMetadataBuilder(IRGenModule &IGM, ClassDecl *theClass,
                              ConstantStructBuilder &builder,
                              const StructLayout &layout,
                              const ClassLayout &fieldLayout)
      : super(IGM, theClass, builder, layout, fieldLayout) {}
  };

  /// A builder for resilient, non-generic class metadata.
  class ResilientClassMetadataBuilder :
      public ConcreteClassMetadataBuilderBase<ResilientClassMetadataBuilder,
                                              ResilientClassMemberBuilder> {
    using super = ConcreteClassMetadataBuilderBase<ResilientClassMetadataBuilder,
                                                   ResilientClassMemberBuilder>;

  public:
    ResilientClassMetadataBuilder(IRGenModule &IGM, ClassDecl *theClass,
                                  ConstantStructBuilder &builder,
                                  const StructLayout &layout,
                                  const ClassLayout &fieldLayout)
      : super(IGM, theClass, builder, layout, fieldLayout) {}
  };

  /// A builder for GenericClassMetadataPattern objects.
  class GenericClassMetadataBuilder :
    public GenericMetadataBuilderBase<GenericClassMetadataBuilder,
                      ClassMetadataBuilderBase<GenericClassMetadataBuilder,
                                               ResilientClassMemberBuilder>>
  {
    using super = GenericMetadataBuilderBase;

    Optional<ConstantAggregateBuilderBase::PlaceholderPosition>
      ClassRODataOffset, MetaclassObjectOffset, MetaclassRODataOffset;
  public:
    GenericClassMetadataBuilder(IRGenModule &IGM, ClassDecl *theClass,
                                ConstantStructBuilder &B,
                                const StructLayout &layout,
                                const ClassLayout &fieldLayout)
      : super(IGM, theClass, B, layout, fieldLayout)
    {
      // We need special initialization of metadata objects to trick the ObjC
      // runtime into initializing them.
      HasDependentMetadata = true;
    }

    void layoutHeader() {
      super::layoutHeader();

      // RelativePointer<HeapObjectDestroyer> Destroy;
      addDestructorFunction();

      // RelativePointer<ClassIVarDestroyer> IVarDestroyer;
      addIVarDestroyer();

      // ClassFlags Flags;
      addClassFlags();

      // uint16_t ClassRODataOffset;
      if (IGM.ObjCInterop)
        ClassRODataOffset = B.addPlaceholderWithSize(IGM.Int16Ty);
      else
        B.addInt16(0);

      // uint16_t MetaclassObjectOffset;
      if (IGM.ObjCInterop)
        MetaclassObjectOffset = B.addPlaceholderWithSize(IGM.Int16Ty);
      else
        B.addInt16(0);

      // uint16_t MetadataRODataOffset;
      if (IGM.ObjCInterop)
        MetaclassRODataOffset = B.addPlaceholderWithSize(IGM.Int16Ty);
      else
        B.addInt16(0);

      // uint16_t Reserved;
      B.addInt16(0);
    }

    GenericMetadataPatternFlags getPatternFlags() {
      auto flags = super::getPatternFlags();

      flags.class_setHasImmediateMembersPattern(hasImmediateMembersPattern());

      return flags;
    }

    void emitInstantiationDefinitions() {
      // Emit the base-offset variable.
      emitClassMetadataBaseOffset();

      super::emitInstantiationDefinitions();
    }

    void addDestructorFunction() {
      auto function = getAddrOfDestructorFunction();
      B.addRelativeAddressOrNull(function ? *function : nullptr);
    }

    void addIVarDestroyer() {
      auto function = getAddrOfIVarDestroyer();
      B.addRelativeAddressOrNull(function ? *function : nullptr);
    }

    bool hasExtraDataPattern() {
      return IGM.ObjCInterop;
    }

    PartialPattern buildExtraDataPattern() {
      ConstantInitBuilder subBuilder(IGM);
      auto subB = subBuilder.beginStruct();
      subB.setPacked(true);

      // The offset of the pattern bytes in the overall extra-data section.
      // Any bytes before this will be zeroed.  Currently we don't take
      // advantage of this.
      Size patternOffset = Size(0);

      if (IGM.ObjCInterop) {
        // Add the metaclass object.
        B.fillPlaceholderWithInt(*MetaclassObjectOffset, IGM.Int16Ty,
          IGM.getOffsetInWords(patternOffset + subB.getNextOffsetFromGlobal()));
        addMetaclassObject(subB);

        // Add the RO-data objects.
        auto roDataPoints =
          emitClassPrivateDataFields(IGM, subB, Target);
        B.fillPlaceholderWithInt(*ClassRODataOffset, IGM.Int16Ty,
          IGM.getOffsetInWords(patternOffset + roDataPoints.first));
        B.fillPlaceholderWithInt(*MetaclassRODataOffset, IGM.Int16Ty,
          IGM.getOffsetInWords(patternOffset + roDataPoints.second));
      }

      auto patternSize = subB.getNextOffsetFromGlobal();

      auto global = subB.finishAndCreateGlobal("", IGM.getPointerAlignment(),
                                               /*constant*/ true);

      return { global, patternOffset, patternSize };
    }

    void addMetaclassObject(ConstantStructBuilder &B) {
      // isa
      ClassDecl *rootClass = getRootClassForMetaclass(IGM, Target);
      auto isa = IGM.getAddrOfMetaclassObject(rootClass, NotForDefinition);
      B.add(isa);
      // super, which is dependent if the superclass is generic
      B.addNullPointer(IGM.ObjCClassPtrTy);
      // cache
      B.add(IGM.getObjCEmptyCachePtr());
      // vtable
      B.add(IGM.getObjCEmptyVTablePtr());
      // rodata, which is always dependent
      B.addInt(IGM.IntPtrTy, 0);
    }

    bool hasImmediateMembersPattern() {
      // TODO: use the real field offsets if they're known statically.
      return false;
    }

    llvm::Value *emitAllocateMetadata(IRGenFunction &IGF,
                                      llvm::Value *descriptor,
                                      llvm::Value *arguments,
                                      llvm::Value *templatePointer) {
      auto metadata =
        IGF.Builder.CreateCall(IGM.getAllocateGenericClassMetadataFn(),
                               {descriptor, arguments, templatePointer});

      return metadata;
    }

    bool hasCompletionFunction() {
      // TODO: recognize cases where this is not required.
      // For example, under ObjCInterop mode we can move class realization
      // into the allocation phase if the superclass is trivial and there's
      // no layout to do.
      return true;
    }

    void emitInitializeMetadata(IRGenFunction &IGF,
                                llvm::Value *metadata,
                                bool isVWTMutable,
                                MetadataDependencyCollector *collector) {
      assert(!HasDependentVWT && "class should never have dependent VWT");

      // Install the superclass.  The runtime takes care of installing
      // SwiftObject if we're building with ObjC interop and don't have
      // a formal superclass.
      if (Target->hasSuperclass()) {
        CanType superclass = Target->mapTypeIntoContext(Target->getSuperclass())
                                   ->getCanonicalType();
        emitStoreOfSuperclass(IGF, superclass, metadata, collector);
      }

      // We can assume that this never relocates the metadata because
      // it should have been allocated properly for the class.
      (void) emitFinishInitializationOfClassMetadata(IGF, metadata, collector);
    }
  };
} // end anonymous namespace

/// Emit the ObjC-compatible class symbol for a class.
/// Since LLVM and many system linkers do not have a notion of relative symbol
/// references, we emit the symbol as a global asm block.
static void emitObjCClassSymbol(IRGenModule &IGM,
                                ClassDecl *classDecl,
                                llvm::GlobalValue *metadata) {
  llvm::SmallString<32> classSymbol;
  LinkEntity::forObjCClass(classDecl).mangle(classSymbol);
  
  // Create the alias.
  auto *metadataTy = cast<llvm::PointerType>(metadata->getType());

  // Create the alias.
  auto *alias = llvm::GlobalAlias::create(metadataTy->getElementType(),
                                          metadataTy->getAddressSpace(),
                                          metadata->getLinkage(),
                                          classSymbol.str(), metadata,
                                          IGM.getModule());
  alias->setVisibility(metadata->getVisibility());

  if (IGM.useDllStorage())
    alias->setDLLStorageClass(metadata->getDLLStorageClass());
}

/// Emit the type metadata or metadata template for a class.
void irgen::emitClassMetadata(IRGenModule &IGM, ClassDecl *classDecl,
                              const StructLayout &layout,
                              const ClassLayout &fieldLayout) {
  assert(!classDecl->isForeign());

  // Set up a dummy global to stand in for the metadata object while we produce
  // relative references.
  ConstantInitBuilder builder(IGM);
  auto init = builder.beginStruct();
  init.setPacked(true);

  bool isPattern;
  bool canBeConstant;
  if (classDecl->isGenericContext()) {
    GenericClassMetadataBuilder builder(IGM, classDecl, init,
                                        layout, fieldLayout);
    builder.layout();
    isPattern = true;
    canBeConstant = false;

    builder.createMetadataAccessFunction();
  } else if (doesClassMetadataRequireDynamicInitialization(IGM, classDecl)) {
    ResilientClassMetadataBuilder builder(IGM, classDecl, init,
                                          layout, fieldLayout);
    builder.layout();
    isPattern = false;
    canBeConstant = builder.canBeConstant();

    builder.createMetadataAccessFunction();
  } else {
    FixedClassMetadataBuilder builder(IGM, classDecl, init,
                                      layout, fieldLayout);
    builder.layout();
    isPattern = false;
    canBeConstant = builder.canBeConstant();

    builder.createMetadataAccessFunction();
  }

  CanType declaredType = classDecl->getDeclaredType()->getCanonicalType();

  // For now, all type metadata is directly stored.
  bool isIndirect = false;

  StringRef section{};
  if (classDecl->isObjC() &&
      IGM.TargetInfo.OutputObjectFormat == llvm::Triple::MachO)
    section = "__DATA,__objc_data, regular";

  auto var = IGM.defineTypeMetadata(declaredType, isIndirect, isPattern,
                                    canBeConstant,
                                    init.finishAndCreateFuture(),
                                    section);

  // Add classes that don't require dynamic initialization to the
  // ObjC class list.
  if (IGM.ObjCInterop && !isPattern && !isIndirect &&
      !doesClassMetadataRequireDynamicInitialization(IGM, classDecl)) {
    // Emit the ObjC class symbol to make the class visible to ObjC.
    if (classDecl->isObjC()) {
      emitObjCClassSymbol(IGM, classDecl, var);
    }

    IGM.addObjCClass(var,
              classDecl->getAttrs().hasAttribute<ObjCNonLazyRealizationAttr>());
  }

  IGM.IRGen.noteUseOfAnyParentTypeMetadata(classDecl);
}

llvm::Value *IRGenFunction::emitInvariantLoad(Address address,
                                              const llvm::Twine &name) {
  auto load = Builder.CreateLoad(address, name);
  setInvariantLoad(load);
  return load;
}

void IRGenFunction::setInvariantLoad(llvm::LoadInst *load) {
  load->setMetadata(IGM.InvariantMetadataID, IGM.InvariantNode);
}

void IRGenFunction::setDereferenceableLoad(llvm::LoadInst *load,
                                           unsigned size) {
  auto sizeConstant = llvm::ConstantInt::get(IGM.Int64Ty, size);
  auto sizeNode = llvm::MDNode::get(IGM.LLVMContext,
                                  llvm::ConstantAsMetadata::get(sizeConstant));
  load->setMetadata(IGM.DereferenceableID, sizeNode);
}

/// Emit a load from the given metadata at a constant index.
///
/// The load is marked invariant. This function should not be called
/// on metadata objects that are in the process of being initialized.
static llvm::LoadInst *
emitInvariantLoadFromMetadataAtIndex(IRGenFunction &IGF,
                                     llvm::Value *metadata,
                                     int index,
                                     llvm::Type *objectTy,
                               const Twine &suffix = Twine::createNull()) {
  auto result = emitLoadFromMetadataAtIndex(IGF, metadata, index, objectTy,
                                            suffix);
  IGF.setInvariantLoad(result);
  return result;
}

/// Given a type metadata pointer, load its value witness table.
llvm::Value *
IRGenFunction::emitValueWitnessTableRefForMetadata(llvm::Value *metadata) {
  auto witness = emitInvariantLoadFromMetadataAtIndex(*this, metadata, -1,
                                                      IGM.WitnessTablePtrTy,
                                                      ".valueWitnesses");
  // A value witness table is dereferenceable to the number of value witness
  // pointers.
  
  // TODO: If we know the type statically has extra inhabitants, we know
  // there are more witnesses.
  auto numValueWitnesses
    = unsigned(ValueWitness::Last_RequiredValueWitness) + 1;
  setDereferenceableLoad(witness,
                         IGM.getPointerSize().getValue() * numValueWitnesses);
  return witness;
}

/// Given a lowered SIL type, load a value witness table that represents its
/// layout.
llvm::Value *
IRGenFunction::emitValueWitnessTableRef(SILType type,
                                        llvm::Value **metadataSlot) {
  return emitValueWitnessTableRef(type, MetadataState::Complete, metadataSlot);
}

llvm::Value *
IRGenFunction::emitValueWitnessTableRef(SILType type,
                                        DynamicMetadataRequest request,
                                        llvm::Value **metadataSlot) {
  assert(request.canResponseStatusBeIgnored());
  assert(!request.isStaticallyAbstract() &&
         "cannot make an abstract request for a value witness table");

  // See if we have a cached projection we can use.
  if (auto cached = tryGetLocalTypeDataForLayout(type,
                                  LocalTypeDataKind::forValueWitnessTable())) {
    if (metadataSlot)
      *metadataSlot = emitTypeMetadataRefForLayout(type, request);
    return cached;
  }
  
  auto metadata = emitTypeMetadataRefForLayout(type, request);
  if (metadataSlot) *metadataSlot = metadata;
  auto vwtable = emitValueWitnessTableRefForMetadata(metadata);
  setScopedLocalTypeDataForLayout(type,
                                  LocalTypeDataKind::forValueWitnessTable(),
                                  vwtable);
  return vwtable;
}

//===----------------------------------------------------------------------===//
// Value types (structs and enums)
//===----------------------------------------------------------------------===//

static llvm::Value *
emitInPlaceValueTypeMetadataInitialization(IRGenFunction &IGF,
                                           CanNominalType type,
                                           llvm::Value *metadata,
                                   MetadataDependencyCollector *collector) {
  // All the value types are basically similar, as are foreign types.
  assert(isa<StructType>(type) || isa<EnumType>(type) ||
         IGF.IGM.requiresForeignTypeMetadata(type));

  // Set up the value witness table if it's dependent.
  SILType loweredType = IGF.IGM.getLoweredType(AbstractionPattern(type), type);
  auto &ti = IGF.IGM.getTypeInfo(loweredType);
  if (!ti.isFixedSize()) {
    loweredType = loweredType.getAddressType();
    if (isa<StructType>(type)) {
      emitInitializeFieldOffsetVector(IGF, loweredType, metadata, true,
                                      collector);
    } else if (isa<EnumType>(type)) {
      auto &strategy = getEnumImplStrategy(IGF.IGM, loweredType);
      strategy.initializeMetadata(IGF, metadata, true, loweredType, collector);
    }
  }

  return metadata;
}

/// Create an access function for the type metadata of the given
/// non-generic nominal type.
static void createInPlaceValueTypeMetadataAccessFunction(IRGenModule &IGM,
                                                      NominalTypeDecl *typeDecl) {
  assert(!typeDecl->isGenericContext());
  auto type =
    cast<NominalType>(typeDecl->getDeclaredType()->getCanonicalType());

  (void) getTypeMetadataAccessFunction(IGM, type, ForDefinition,
                                       [&](IRGenFunction &IGF,
                                           DynamicMetadataRequest request,
                                           llvm::Constant *cacheVariable) {
    return emitInPlaceTypeMetadataAccessFunctionBody(IGF, type, cacheVariable,
      [&](IRGenFunction &IGF, llvm::Value *metadata) {
        MetadataDependencyCollector *collector = nullptr; // FIXME
        return emitInPlaceValueTypeMetadataInitialization(IGF, type, metadata,
                                                          collector);
      });
  });
}

//===----------------------------------------------------------------------===//
// Structs
//===----------------------------------------------------------------------===//

namespace {
  /// An adapter for laying out struct metadata.
  template <class Impl>
  class StructMetadataBuilderBase : public StructMetadataVisitor<Impl> {
    using super = StructMetadataVisitor<Impl>;

  protected:
    ConstantStructBuilder &B;
    using super::IGM;
    using super::Target;
    using super::asImpl;

    StructMetadataBuilderBase(IRGenModule &IGM, StructDecl *theStruct,
                              ConstantStructBuilder &B)
      : super(IGM, theStruct), B(B) {
    }

  public:
    void noteStartOfTypeSpecificMembers() {}

    SILType getLoweredType() {
      return IGM.getLoweredType(Target->getDeclaredTypeInContext());
    }

    MetadataKind getMetadataKind() {
      return MetadataKind::Struct;
    }

    void addMetadataFlags() {
      B.addInt(IGM.MetadataKindTy, unsigned(getMetadataKind()));
    }

    llvm::Constant *emitNominalTypeDescriptor() {
      auto descriptor =
        StructContextDescriptorBuilder(IGM, Target, RequireMetadata).emit();
      return descriptor;
    }

    void addNominalTypeDescriptor() {
      B.add(emitNominalTypeDescriptor());
    }

    llvm::Constant *emitValueWitnessTable() {
      auto type = this->Target->getDeclaredType()->getCanonicalType();
      return irgen::emitValueWitnessTable(IGM, type, false);
    }

    void addValueWitnessTable() {
      B.add(emitValueWitnessTable());
    }

    void addFieldOffset(VarDecl *var) {
      assert(var->hasStorage() &&
             "storing field offset for computed property?!");
      SILType structType = getLoweredType();

      llvm::Constant *offset =
        emitPhysicalStructMemberFixedOffset(IGM, structType, var);
      // If we have a fixed offset, add it. Otherwise, leave zero as a
      // placeholder.
      if (offset) {
        B.add(offset);
      } else {
        asImpl().flagUnfilledFieldOffset();
        B.addInt(IGM.Int32Ty, 0);
      }
    }

    void noteEndOfFieldOffsets() {
      B.addAlignmentPadding(super::IGM.getPointerAlignment());
    }

    void addGenericArgument(CanType type) {
      B.addNullPointer(IGM.TypeMetadataPtrTy);
    }

    void addGenericWitnessTable(CanType type, ProtocolConformanceRef conf) {
      B.addNullPointer(IGM.WitnessTablePtrTy);
    }
  };

  class StructMetadataBuilder :
    public StructMetadataBuilderBase<StructMetadataBuilder> {

    bool HasUnfilledFieldOffset = false;
  public:
    StructMetadataBuilder(IRGenModule &IGM, StructDecl *theStruct,
                          ConstantStructBuilder &B)
      : StructMetadataBuilderBase(IGM, theStruct, B) {}

    void flagUnfilledFieldOffset() {
      HasUnfilledFieldOffset = true;
    }

    bool canBeConstant() {
      return !HasUnfilledFieldOffset;
    }

    void createMetadataAccessFunction() {
      createInPlaceValueTypeMetadataAccessFunction(IGM, Target);
    }
  };
  
  /// Emit a value witness table for a fixed-layout generic type, or a null
  /// placeholder if the value witness table is dependent on generic parameters.
  /// Returns nullptr if the value witness table is dependent.
  static llvm::Constant *
  getValueWitnessTableForGenericValueType(IRGenModule &IGM,
                                          NominalTypeDecl *decl,
                                          bool &dependent) {
    CanType unboundType
      = decl->getDeclaredType()->getCanonicalType();
    
    dependent = hasDependentValueWitnessTable(IGM, unboundType);
    return emitValueWitnessTable(IGM, unboundType, dependent);
  }
  
  /// A builder for metadata templates.
  class GenericStructMetadataBuilder :
    public GenericValueMetadataBuilderBase<GenericStructMetadataBuilder,
                      StructMetadataBuilderBase<GenericStructMetadataBuilder>> {

    using super = GenericValueMetadataBuilderBase;

  public:
    GenericStructMetadataBuilder(IRGenModule &IGM, StructDecl *theStruct,
                                 ConstantStructBuilder &B)
      : super(IGM, theStruct, B) {}

    llvm::Value *emitAllocateMetadata(IRGenFunction &IGF,
                                      llvm::Value *descriptor,
                                      llvm::Value *arguments,
                                      llvm::Value *templatePointer) {
      auto &layout = IGM.getMetadataLayout(Target);
      auto extraSize = layout.getSize().getOffsetToEnd()
                         - IGM.getOffsetOfStructTypeSpecificMetadataMembers();
      auto extraSizeV = IGM.getSize(extraSize);

      return IGF.Builder.CreateCall(IGM.getAllocateGenericValueMetadataFn(),
                                    {descriptor, arguments, templatePointer,
                                     extraSizeV});
    }

    void flagUnfilledFieldOffset() {
      // We just assume this might happen.
    }
    
    llvm::Constant *emitValueWitnessTable() {
      return getValueWitnessTableForGenericValueType(IGM, Target,
                                                     HasDependentVWT);
    }

    bool hasExtraDataPattern() {
      auto &ti = IGM.getTypeInfo(getLoweredType());
      if (!isa<FixedTypeInfo>(ti))
        return false;

      if (Target->getStoredProperties().empty())
        return false;

      return true;
    }

    /// Fill in a constant field offset vector if possible.
    PartialPattern buildExtraDataPattern() {
      ConstantInitBuilder builder(IGM);
      auto init = builder.beginArray(IGM.Int32Ty);

      struct Scanner : StructMetadataScanner<Scanner> {
        SILType Type;
        ConstantArrayBuilder &B;
        Scanner(IRGenModule &IGM, StructDecl *target, SILType type,
                ConstantArrayBuilder &B)
          : StructMetadataScanner(IGM, target), Type(type), B(B) {}

        void addFieldOffset(VarDecl *field) {
          auto offset = emitPhysicalStructMemberFixedOffset(IGM, Type, field);
          if (offset) {
            B.add(offset);
            return;
          }
          assert(IGM.isKnownEmpty(Type.getFieldType(field, IGM.getSILModule()),
                                  ResilienceExpansion::Maximal));
          B.addInt32(0);
        }

        void noteEndOfFieldOffsets() {
          B.addAlignmentPadding(IGM.getPointerAlignment());
        }
      };
      Scanner(IGM, Target, getLoweredType(), init).layout();
      Size vectorSize = init.getNextOffsetFromGlobal();

      auto global = init.finishAndCreateGlobal("", IGM.getPointerAlignment(),
                                               /*constant*/ true);

      auto &layout = IGM.getMetadataLayout(Target);
      return { global,
               layout.getFieldOffsetVectorOffset().getStatic()
                 - IGM.getOffsetOfStructTypeSpecificMetadataMembers(),
               vectorSize };
    }

    bool hasCompletionFunction() {
      return !isa<FixedTypeInfo>(IGM.getTypeInfo(getLoweredType()));
    }

    void emitInitializeMetadata(IRGenFunction &IGF,
                                llvm::Value *metadata,
                                bool isVWTMutable,
                                MetadataDependencyCollector *collector) {
      auto loweredTy = getLoweredType();
      auto &fixedTI = IGM.getTypeInfo(loweredTy);
      if (isa<FixedTypeInfo>(fixedTI)) return;

      emitInitializeFieldOffsetVector(IGF, loweredTy, metadata, isVWTMutable,
                                      collector);
    }
  };
} // end anonymous namespace

/// Emit the type metadata or metadata template for a struct.
void irgen::emitStructMetadata(IRGenModule &IGM, StructDecl *structDecl) {
  // TODO: structs nested within generic types
  ConstantInitBuilder initBuilder(IGM);
  auto init = initBuilder.beginStruct();
  init.setPacked(true);

  bool isPattern;
  bool canBeConstant;
  if (structDecl->isGenericContext()) {
    GenericStructMetadataBuilder builder(IGM, structDecl, init);
    builder.layout();
    isPattern = true;
    canBeConstant = false;

    builder.createMetadataAccessFunction();
  } else {
    StructMetadataBuilder builder(IGM, structDecl, init);
    builder.layout();
    isPattern = false;
    canBeConstant = builder.canBeConstant();

    builder.createMetadataAccessFunction();
  }

  CanType declaredType = structDecl->getDeclaredType()->getCanonicalType();

  // For now, all type metadata is directly stored.
  bool isIndirect = false;

  IGM.defineTypeMetadata(declaredType, isIndirect, isPattern,
                         canBeConstant, init.finishAndCreateFuture());

  IGM.IRGen.noteUseOfAnyParentTypeMetadata(structDecl);
}

void IRGenerator::noteUseOfAnyParentTypeMetadata(NominalTypeDecl *type) {
  // If this is a nested type we also potentially might need the outer types.
  auto *declCtxt = type->getDeclContext();
  auto *parentNominalDecl =
    declCtxt->getAsNominalTypeOrNominalTypeExtensionContext();
  if (!parentNominalDecl)
    return;

  noteUseOfTypeMetadata(parentNominalDecl);
}

// Enums

namespace {

  template<class Impl>
  class EnumMetadataBuilderBase : public EnumMetadataVisitor<Impl> {
    using super = EnumMetadataVisitor<Impl>;

  protected:
    ConstantStructBuilder &B;
    using super::IGM;
    using super::Target;

    EnumMetadataBuilderBase(IRGenModule &IGM, EnumDecl *theEnum,
                            ConstantStructBuilder &B)
      : super(IGM, theEnum), B(B) {
    }

    SILType getLoweredType() {
      return IGM.getLoweredType(Target->getDeclaredTypeInContext());
    }

  public:
    void noteStartOfTypeSpecificMembers() {}

    MetadataKind getMetadataKind() {
      return Target->isOptionalDecl() ? MetadataKind::Optional
                                      : MetadataKind::Enum;
    }

    void addMetadataFlags() {
      auto kind = getMetadataKind();
      B.addInt(IGM.MetadataKindTy, unsigned(kind));
    }

    llvm::Constant *emitValueWitnessTable() {
      auto type = Target->getDeclaredType()->getCanonicalType();
      return irgen::emitValueWitnessTable(IGM, type, false);
    }

    void addValueWitnessTable() {
      B.add(emitValueWitnessTable());
    }

    llvm::Constant *emitNominalTypeDescriptor() {
      auto descriptor =
        EnumContextDescriptorBuilder(IGM, Target, RequireMetadata).emit();
      return descriptor;
    }

    void addNominalTypeDescriptor() {
      B.add(emitNominalTypeDescriptor());
    }

    void addGenericArgument(CanType type) {
      B.addNullPointer(IGM.TypeMetadataPtrTy);
    }

    void addGenericWitnessTable(CanType type, ProtocolConformanceRef conf) {
      B.addNullPointer(IGM.WitnessTablePtrTy);
    }

    Optional<Size> getConstantPayloadSize() {
      auto enumTy = Target->getDeclaredTypeInContext()->getCanonicalType();
      auto &enumTI = IGM.getTypeInfoForUnlowered(enumTy);
      if (!enumTI.isFixedSize(ResilienceExpansion::Maximal)) {
        return None;
      }

      assert(!enumTI.isFixedSize(ResilienceExpansion::Minimal) &&
             "non-generic, non-resilient enums don't need payload size in metadata");
      auto &strategy = getEnumImplStrategy(IGM, enumTy);
      return Size(strategy.getPayloadSizeForMetadata());
    }
  };

  class EnumMetadataBuilder
    : public EnumMetadataBuilderBase<EnumMetadataBuilder> {
    bool HasUnfilledPayloadSize = false;

  public:
    EnumMetadataBuilder(IRGenModule &IGM, EnumDecl *theEnum,
                        ConstantStructBuilder &B)
      : EnumMetadataBuilderBase(IGM, theEnum, B) {}



    void addPayloadSize() {
      auto payloadSize = getConstantPayloadSize();
      if (!payloadSize) {
        B.addInt(IGM.IntPtrTy, 0);
        HasUnfilledPayloadSize = true;
        return;
      }

      B.addInt(IGM.IntPtrTy, payloadSize->getValue());
    }

    bool canBeConstant() {
      return !HasUnfilledPayloadSize;
    }

    void createMetadataAccessFunction() {
      createInPlaceValueTypeMetadataAccessFunction(IGM, Target);
    }
  };

  class GenericEnumMetadataBuilder
    : public GenericValueMetadataBuilderBase<GenericEnumMetadataBuilder,
                          EnumMetadataBuilderBase<GenericEnumMetadataBuilder>>
  {
  public:
    using super = GenericValueMetadataBuilderBase;

    GenericEnumMetadataBuilder(IRGenModule &IGM, EnumDecl *theEnum,
                               ConstantStructBuilder &B)
      : super(IGM, theEnum, B) {}

    llvm::Value *emitAllocateMetadata(IRGenFunction &IGF,
                                      llvm::Value *descriptor,
                                      llvm::Value *arguments,
                                      llvm::Value *templatePointer) {
      auto &layout = IGM.getMetadataLayout(Target);
      auto extraSize = layout.getSize().getOffsetToEnd()
                         - IGM.getOffsetOfEnumTypeSpecificMetadataMembers();
      auto extraSizeV = IGM.getSize(extraSize);

      auto metadata =
        IGF.Builder.CreateCall(IGM.getAllocateGenericValueMetadataFn(),
                               {descriptor, arguments, templatePointer,
                                extraSizeV});

      // Initialize the payload-size field if we have a constant value for it.
      // This is so small that we just do it inline instead of bothering
      // with a pattern.
      if (layout.hasPayloadSizeOffset()) {
        if (auto size = getConstantPayloadSize()) {
          auto offset = layout.getPayloadSizeOffset();
          auto slot = IGF.emitAddressAtOffset(metadata, offset, IGM.SizeTy,
                                              IGM.getPointerAlignment());
          IGF.Builder.CreateStore(IGM.getSize(*size), slot);
        }
      }

      return metadata;
    }

    llvm::Constant *emitValueWitnessTable() {
      return getValueWitnessTableForGenericValueType(IGM, Target,
                                                     HasDependentVWT);
    }

    bool hasCompletionFunction() {
      return !isa<FixedTypeInfo>(IGM.getTypeInfo(getLoweredType()));
    }

    void emitInitializeMetadata(IRGenFunction &IGF,
                                llvm::Value *metadata,
                                bool isVWTMutable,
                                MetadataDependencyCollector *collector) {
      // Nominal types are always preserved through SIL lowering.
      auto enumTy = getLoweredType();

      auto &strategy = getEnumImplStrategy(IGF.IGM, enumTy);
      strategy.initializeMetadata(IGF, metadata, isVWTMutable, enumTy,
                                  collector);
    }
  };

} // end anonymous namespace

void irgen::emitEnumMetadata(IRGenModule &IGM, EnumDecl *theEnum) {
  // TODO: enums nested inside generic types
  ConstantInitBuilder initBuilder(IGM);
  auto init = initBuilder.beginStruct();
  init.setPacked(true);
  
  bool isPattern;
  bool canBeConstant;
  if (theEnum->isGenericContext()) {
    GenericEnumMetadataBuilder builder(IGM, theEnum, init);
    builder.layout();
    isPattern = true;
    canBeConstant = false;

    builder.createMetadataAccessFunction();
  } else {
    EnumMetadataBuilder builder(IGM, theEnum, init);
    builder.layout();
    isPattern = false;
    canBeConstant = builder.canBeConstant();

    builder.createMetadataAccessFunction();
  }

  CanType declaredType = theEnum->getDeclaredType()->getCanonicalType();

  // For now, all type metadata is directly stored.
  bool isIndirect = false;
  
  IGM.defineTypeMetadata(declaredType, isIndirect, isPattern,
                         canBeConstant, init.finishAndCreateFuture());

  IGM.IRGen.noteUseOfAnyParentTypeMetadata(theEnum);
}

llvm::Value *IRGenFunction::emitObjCSelectorRefLoad(StringRef selector) {
  llvm::Constant *loadSelRef = IGM.getAddrOfObjCSelectorRef(selector);
  llvm::Value *loadSel =
    Builder.CreateLoad(Address(loadSelRef, IGM.getPointerAlignment()));

  // When generating JIT'd code, we need to call sel_registerName() to force
  // the runtime to unique the selector. For non-JIT'd code, the linker will
  // do it for us.
  if (IGM.IRGen.Opts.UseJIT) {
    loadSel = Builder.CreateCall(IGM.getObjCSelRegisterNameFn(), loadSel);
  }

  return loadSel;
}

//===----------------------------------------------------------------------===//
// Foreign types
//===----------------------------------------------------------------------===//

namespace {
  /// An adapter that turns a metadata layout class into a foreign metadata
  /// layout class.
  ///
  /// Foreign metadata is generated for declarations that are
  /// synthesized by the Clang importer from C declarations, meaning they don't
  /// have a single Swift binary that is responsible for their emission.
  /// In this case, we emit the record into every binary that needs it, with
  /// a header with a unique identifier string that the runtime can use to pick
  /// the first-used instance as the canonical instance for a process.
  template<typename Impl, typename Base>
  class ForeignMetadataBuilderBase : public Base {
    using super = Base;

  protected:
    using super::IGM;
    using super::asImpl;
    using super::B;

    template <class... T>
    ForeignMetadataBuilderBase(T &&...args) : super(std::forward<T>(args)...) {}

    Size AddressPoint = Size::invalid();

  public:
    void layout() {
      if (asImpl().requiresInitializationFunction())
        asImpl().addInitializationFunction();
      asImpl().addForeignFlags();
      super::layout();
    }
    
    void addForeignFlags() {
      int64_t flags = 0;
      if (asImpl().requiresInitializationFunction()) flags |= 1;
      B.addInt(IGM.IntPtrTy, flags);
    }

    void addForeignName() {
      CanType targetType = asImpl().getTargetType();
      IRGenMangler mangler;
      std::string Name =
        mangler.mangleTypeForForeignMetadataUniquing(targetType);
      llvm::Constant *nameStr = IGM.getAddrOfGlobalString(Name,
                                                 /*relatively addressed*/ true);
      B.addRelativeAddress(nameStr);
    }

    void addInitializationFunction() {
      auto type = cast<NominalType>(asImpl().getTargetType());

      auto fnTy = llvm::FunctionType::get(IGM.VoidTy, {IGM.TypeMetadataPtrTy},
                                          /*variadic*/ false);
      llvm::Function *fn = llvm::Function::Create(fnTy,
                                           llvm::GlobalValue::PrivateLinkage,
                                           Twine("initialize_metadata_")
                                             + type->getDecl()->getName().str(),
                                           &IGM.Module);
      fn->setAttributes(IGM.constructInitialAttributes());
      
      // Set up the function.
      IRGenFunction IGF(IGM, fn);
      if (IGM.DebugInfo)
        IGM.DebugInfo->emitArtificialFunction(IGF, fn);

      // Emit the initialization.
      llvm::Value *metadata = IGF.collectParameters().claimNext();
      asImpl().emitInitialization(IGF, metadata);

      IGF.Builder.CreateRetVoid();

      B.addRelativeAddress(fn);
      
      // Keep pointer alignment on 64-bit platforms for further fields.
      switch (IGM.getPointerSize().getValue()) {
      case 4:
        break;
      case 8:
        B.addInt32(0);
        break;
      default:
        llvm_unreachable("unsupported word size");
      }
    }
    
    void noteAddressPoint() {
      AddressPoint = B.getNextOffsetFromGlobal();
    }

    Size getOffsetOfAddressPoint() const { return AddressPoint; }

    void createMetadataAccessFunction() {
      auto type = cast<NominalType>(asImpl().getTargetType());

      (void) getTypeMetadataAccessFunction(IGM, type, ForDefinition,
                                           [&](IRGenFunction &IGF,
                                               DynamicMetadataRequest request,
                                               llvm::Constant *cacheVariable) {
        return emitInPlaceTypeMetadataAccessFunctionBody(IGF, type,
                                                         cacheVariable,
          [&](IRGenFunction &IGF, llvm::Value *candidate) {
            MetadataDependencyCollector *collector = nullptr;
            auto metadata = uniqueForeignTypeMetadataRef(IGF, candidate);
            return emitInPlaceValueTypeMetadataInitialization(IGF, type,
                                                              metadata,
                                                              collector);
          });
      });
    }
  };

  class ForeignClassMetadataBuilder;
  class ForeignClassMetadataBuilderBase :
      public ForeignClassMetadataVisitor<ForeignClassMetadataBuilder> {
  protected:
    ConstantStructBuilder &B;

    ForeignClassMetadataBuilderBase(IRGenModule &IGM, ClassDecl *target,
                                    ConstantStructBuilder &B)
      : ForeignClassMetadataVisitor(IGM, target), B(B) {}
  };

  /// A builder for ForeignClassMetadata.
  class ForeignClassMetadataBuilder :
      public ForeignMetadataBuilderBase<ForeignClassMetadataBuilder,
                                        ForeignClassMetadataBuilderBase> {
  public:
    ForeignClassMetadataBuilder(IRGenModule &IGM, ClassDecl *target,
                                ConstantStructBuilder &B)
      : ForeignMetadataBuilderBase(IGM, target, B) {}

    void emitInitialization(IRGenFunction &IGF, llvm::Value *metadata) {
      // Dig out the address of the superclass field.
      auto &layout = IGF.IGM.getForeignMetadataLayout(Target);
      Address metadataWords(IGF.Builder.CreateBitCast(metadata,
                                                      IGM.Int8PtrPtrTy),
                            IGM.getPointerAlignment());
      auto superclassField =
        createPointerSizedGEP(IGF, metadataWords,
                              layout.getSuperClassOffset().getStaticOffset());
      superclassField =
        IGF.Builder.CreateBitCast(
                          superclassField,
                          llvm::PointerType::get(IGM.TypeMetadataPtrTy, 0));

      // Unique the superclass field and write it back.
      auto superclass = IGF.Builder.CreateLoad(superclassField);
      auto uniquedSuperclass = uniqueForeignTypeMetadataRef(IGF, superclass);
      IGF.Builder.CreateStore(uniquedSuperclass, superclassField);
    }

    // Visitor methods.

    void addValueWitnessTable() {
      // Without Objective-C interop, foreign classes must still use
      // Swift native reference counting.
      auto type = (IGM.ObjCInterop
                   ? IGM.Context.TheUnknownObjectType
                   : IGM.Context.TheNativeObjectType);
      auto wtable = IGM.getAddrOfValueWitnessTable(type);
      B.add(wtable);
    }

    void addMetadataFlags() {
      B.addInt(IGM.MetadataKindTy, (unsigned) MetadataKind::ForeignClass);
    }

    void addNominalTypeDescriptor() {
      auto descriptor =
        ClassContextDescriptorBuilder(this->IGM, Target, RequireMetadata).emit();
      B.add(descriptor);
    }

    void noteStartOfSuperClass() { }

    void addSuperClass() {
      auto superclassDecl = Target->getSuperclassDecl();
      if (!superclassDecl || !superclassDecl->isForeign()) {
        B.addNullPointer(IGM.TypeMetadataPtrTy);
        return;
      }

      auto superclassType =
        superclassDecl->swift::TypeDecl::getDeclaredInterfaceType()
          ->getCanonicalType();
      auto superclass =
        IGM.getAddrOfForeignTypeMetadataCandidate(superclassType);
      B.add(superclass);
    }

    void addReservedWord() {
      B.addNullPointer(IGM.Int8PtrTy);
    }
  };
  
  /// A builder for ForeignStructMetadata.
  class ForeignStructMetadataBuilder :
    public ForeignMetadataBuilderBase<ForeignStructMetadataBuilder,
                      StructMetadataBuilderBase<ForeignStructMetadataBuilder>>
  {
  public:
    ForeignStructMetadataBuilder(IRGenModule &IGM, StructDecl *target,
                                 ConstantStructBuilder &builder)
        : ForeignMetadataBuilderBase(IGM, target, builder) {}
    
    CanType getTargetType() const {
      return Target->getDeclaredType()->getCanonicalType();
    }

    bool requiresInitializationFunction() const {
      return false;
    }
    void emitInitialization(IRGenFunction &IGF, llvm::Value *metadata) {}

    void addValueWitnessTable() {
      B.add(emitValueWitnessTable());
    }

    void flagUnfilledFieldOffset() {
      llvm_unreachable("foreign type with non-fixed layout?");
    }
  };
  
  /// A builder for ForeignEnumMetadata.
  class ForeignEnumMetadataBuilder :
    public ForeignMetadataBuilderBase<ForeignEnumMetadataBuilder,
                      EnumMetadataBuilderBase<ForeignEnumMetadataBuilder>>
  {
  public:
    ForeignEnumMetadataBuilder(IRGenModule &IGM, EnumDecl *target,
                               ConstantStructBuilder &builder)
      : ForeignMetadataBuilderBase(IGM, target, builder) {}
    
    CanType getTargetType() const {
      return Target->getDeclaredType()->getCanonicalType();
    }

    bool requiresInitializationFunction() const {
      return false;
    }
    void emitInitialization(IRGenFunction &IGF, llvm::Value *metadata) {}

    void addValueWitnessTable() {
      B.add(emitValueWitnessTable());
    }
    
    void addPayloadSize() const {
      llvm_unreachable("nongeneric enums shouldn't need payload size in metadata");
    }
  };
} // end anonymous namespace

bool IRGenModule::requiresForeignTypeMetadata(CanType type) {
  if (NominalTypeDecl *nominal = type->getAnyNominal()) {
    if (auto *clas = dyn_cast<ClassDecl>(nominal)) {
      return clas->isForeign();
    }

    return isa<ClangModuleUnit>(nominal->getModuleScopeContext());
  }

  return false;
}

llvm::Constant *
IRGenModule::getAddrOfForeignTypeMetadataCandidate(CanType type) {
  // What we save in GlobalVars is actually the offsetted value.
  auto entity = LinkEntity::forForeignTypeMetadataCandidate(type);
  if (auto entry = GlobalVars[entity])
    return entry;

  // Create a temporary base for relative references.
  ConstantInitBuilder builder(*this);
  auto init = builder.beginStruct();
  init.setPacked(true);

  // Local function to create the global variable for the foreign type
  // metadata candidate.
  Size addressPoint;
  llvm::Constant *result = nullptr;
  auto createCandidateVariable = [&] {
    auto definition = init.finishAndCreateFuture();

    // Create the global variable.
    LinkInfo link = LinkInfo::get(*this, entity, ForDefinition);
    auto var =
        createVariable(*this, link, definition.getType(),
                       getPointerAlignment());
    definition.installInGlobal(var);

    // Apply the offset.
    result = llvm::ConstantExpr::getBitCast(var, Int8PtrTy);
    result = llvm::ConstantExpr::getInBoundsGetElementPtr(
        Int8Ty, result, getSize(addressPoint));
    result = llvm::ConstantExpr::getBitCast(result, TypeMetadataPtrTy);

    // Only remember the offset.
    GlobalVars[entity] = result;
  };

  // Compute the constant initializer and the offset of the type
  // metadata candidate within it.
  if (auto classType = dyn_cast<ClassType>(type)) {
    assert(!classType.getParent());
    auto classDecl = classType->getDecl();
    assert(classDecl->getForeignClassKind() == ClassDecl::ForeignKind::CFType);

    ForeignClassMetadataBuilder builder(*this, classDecl, init);
    builder.layout();
    addressPoint = builder.getOffsetOfAddressPoint();

    createCandidateVariable();
    builder.createMetadataAccessFunction();
  } else if (auto structType = dyn_cast<StructType>(type)) {
    auto structDecl = structType->getDecl();
    assert(isa<ClangModuleUnit>(structDecl->getModuleScopeContext()));

    ImportedStructs.insert(structDecl);

    ForeignStructMetadataBuilder builder(*this, structDecl, init);
    builder.layout();
    addressPoint = builder.getOffsetOfAddressPoint();

    createCandidateVariable();
    builder.createMetadataAccessFunction();
  } else if (auto enumType = dyn_cast<EnumType>(type)) {
    auto enumDecl = enumType->getDecl();
    assert(enumDecl->hasClangNode());
    
    ForeignEnumMetadataBuilder builder(*this, enumDecl, init);
    builder.layout();
    addressPoint = builder.getOffsetOfAddressPoint();

    createCandidateVariable();
    builder.createMetadataAccessFunction();
  } else {
    llvm_unreachable("foreign metadata for unexpected type?!");
  }

  // Keep type metadata around for all types.
  addRuntimeResolvableType(type->getAnyNominal());
  
  // If the enclosing type is also an imported type, force its metadata too.
  if (auto enclosing = type->getNominalParent()) {
    auto canonicalEnclosing = enclosing->getCanonicalType();
    if (requiresForeignTypeMetadata(canonicalEnclosing)) {
      (void)getTypeMetadataAccessFunction(*this, canonicalEnclosing,
                                          ForDefinition);
    }
  }

  return result;
}

// Protocols

/// Get the runtime identifier for a special protocol, if any.
SpecialProtocol irgen::getSpecialProtocolID(ProtocolDecl *P) {
  auto known = P->getKnownProtocolKind();
  if (!known)
    return SpecialProtocol::None;
  switch (*known) {
  case KnownProtocolKind::Error:
    return SpecialProtocol::Error;
    
  // The other known protocols aren't special at runtime.
  case KnownProtocolKind::Sequence:
  case KnownProtocolKind::IteratorProtocol:
  case KnownProtocolKind::RawRepresentable:
  case KnownProtocolKind::Equatable:
  case KnownProtocolKind::Hashable:
  case KnownProtocolKind::CaseIterable:
  case KnownProtocolKind::Comparable:
  case KnownProtocolKind::ObjectiveCBridgeable:
  case KnownProtocolKind::DestructorSafeContainer:
  case KnownProtocolKind::SwiftNewtypeWrapper:
  case KnownProtocolKind::ExpressibleByArrayLiteral:
  case KnownProtocolKind::ExpressibleByBooleanLiteral:
  case KnownProtocolKind::ExpressibleByDictionaryLiteral:
  case KnownProtocolKind::ExpressibleByExtendedGraphemeClusterLiteral:
  case KnownProtocolKind::ExpressibleByFloatLiteral:
  case KnownProtocolKind::ExpressibleByIntegerLiteral:
  case KnownProtocolKind::ExpressibleByStringInterpolation:
  case KnownProtocolKind::ExpressibleByStringLiteral:
  case KnownProtocolKind::ExpressibleByNilLiteral:
  case KnownProtocolKind::ExpressibleByUnicodeScalarLiteral:
  case KnownProtocolKind::ExpressibleByColorLiteral:
  case KnownProtocolKind::ExpressibleByImageLiteral:
  case KnownProtocolKind::ExpressibleByFileReferenceLiteral:
  case KnownProtocolKind::ExpressibleByBuiltinBooleanLiteral:
  case KnownProtocolKind::ExpressibleByBuiltinUTF16ExtendedGraphemeClusterLiteral:
  case KnownProtocolKind::ExpressibleByBuiltinExtendedGraphemeClusterLiteral:
  case KnownProtocolKind::ExpressibleByBuiltinFloatLiteral:
  case KnownProtocolKind::ExpressibleByBuiltinIntegerLiteral:
  case KnownProtocolKind::ExpressibleByBuiltinStringLiteral:
  case KnownProtocolKind::ExpressibleByBuiltinUTF16StringLiteral:
  case KnownProtocolKind::ExpressibleByBuiltinUnicodeScalarLiteral:
  case KnownProtocolKind::OptionSet:
  case KnownProtocolKind::BridgedNSError:
  case KnownProtocolKind::BridgedStoredNSError:
  case KnownProtocolKind::CFObject:
  case KnownProtocolKind::ErrorCodeProtocol:
  case KnownProtocolKind::ExpressibleByBuiltinConstStringLiteral:
  case KnownProtocolKind::ExpressibleByBuiltinConstUTF16StringLiteral:
  case KnownProtocolKind::CodingKey:
  case KnownProtocolKind::Encodable:
  case KnownProtocolKind::Decodable:
    return SpecialProtocol::None;
  }

  llvm_unreachable("Not a valid KnownProtocolKind.");
}

namespace {
  class ProtocolDescriptorBuilder {
    IRGenModule &IGM;
    ConstantStructBuilder &B;
    ProtocolDecl *Protocol;
    std::string AssociatedTypeNames;
    SILDefaultWitnessTable *DefaultWitnesses;

  public:
    ProtocolDescriptorBuilder(IRGenModule &IGM, ProtocolDecl *protocol,
                              ConstantStructBuilder &B,
                              SILDefaultWitnessTable *defaultWitnesses)
      : IGM(IGM), B(B), Protocol(protocol),
        DefaultWitnesses(defaultWitnesses) {}

    void layout() {
      addObjCCompatibilityIsa();
      addName();
      addInherited();
      addObjCCompatibilityTables();
      addSize();
      addFlags();
      addRequirements();
      addSuperclass();
      addAssociatedTypeNames();

      B.suggestType(IGM.ProtocolDescriptorStructTy);
    }

    void addObjCCompatibilityIsa() {
      // The ObjC runtime will drop a reference to its magic Protocol class
      // here.
      B.addNullPointer(IGM.Int8PtrTy);
    }
    
    void addName() {
      // Include the _Tt prefix. Since Swift protocol descriptors are laid
      // out to look like ObjC Protocol* objects, the name has to clearly be
      // a Swift mangled name.

      IRGenMangler mangler;
      std::string Name =
        mangler.mangleForProtocolDescriptor(Protocol->getDeclaredType());

      auto global = IGM.getAddrOfGlobalString(Name);
      B.add(global);
    }
    
    void addInherited() {
      // If there are no inherited protocols, produce null.
      auto inherited = Protocol->getInheritedProtocols();
      if (inherited.empty()) {
        B.addNullPointer(IGM.Int8PtrTy);
        return;
      }
      
      // Otherwise, collect references to all of the inherited protocol
      // descriptors.
      SmallVector<llvm::Constant*, 4> inheritedDescriptors;
      inheritedDescriptors.push_back(IGM.getSize(Size(inherited.size())));
      
      for (ProtocolDecl *p : inherited) {
        auto descriptor = IGM.getAddrOfProtocolDescriptor(p);
        inheritedDescriptors.push_back(descriptor);
      }
      
      auto inheritedInit = llvm::ConstantStruct::getAnon(inheritedDescriptors);
      auto inheritedVar = new llvm::GlobalVariable(IGM.Module,
                                           inheritedInit->getType(),
                                           /*isConstant*/ true,
                                           llvm::GlobalValue::PrivateLinkage,
                                           inheritedInit);
      
      B.addBitCast(inheritedVar, IGM.Int8PtrTy);
    }
    
    void addObjCCompatibilityTables() {
      // Required instance methods
      B.addNullPointer(IGM.Int8PtrTy);
      // Required class methods
      B.addNullPointer(IGM.Int8PtrTy);
      // Optional instance methods
      B.addNullPointer(IGM.Int8PtrTy);
      // Optional class methods
      B.addNullPointer(IGM.Int8PtrTy);
      // Properties
      B.addNullPointer(IGM.Int8PtrTy);
    }
    
    void addSize() {
      // The number of fields so far in words, plus 4 bytes for size and
      // 4 bytes for flags.
      B.addInt32(B.getNextOffsetFromGlobal().getValue() + 4 + 4);
    }
    
    void addFlags() {
      auto flags = ProtocolDescriptorFlags()
        .withSwift(true)
        .withClassConstraint(Protocol->requiresClass()
                               ? ProtocolClassConstraint::Class
                               : ProtocolClassConstraint::Any)
        .withDispatchStrategy(
                Lowering::TypeConverter::getProtocolDispatchStrategy(Protocol))
        .withSpecialProtocol(getSpecialProtocolID(Protocol));

      if (DefaultWitnesses)
        flags = flags.withResilient(true);

      B.addInt32(flags.getIntValue());
    }

    void addRequirements() {
      auto &pi = IGM.getProtocolInfo(Protocol);

      B.addInt32(pi.getNumWitnesses());

      // If there are no entries, just add a null reference and return.
      if (pi.getNumWitnesses() == 0) {
        B.addInt(IGM.RelativeAddressTy, 0);
        return;
      }

      ConstantInitBuilder reqtBuilder(IGM);
      auto reqtsArray = reqtBuilder.beginArray(IGM.ProtocolRequirementStructTy);
      for (auto &entry : pi.getWitnessEntries()) {
        auto reqt = reqtsArray.beginStruct(IGM.ProtocolRequirementStructTy);

        auto info = getRequirementInfo(entry);

        // Flags.
        reqt.addInt32(info.Flags.getIntValue());

        // Dispatch thunk.
        reqt.addRelativeAddressOrNull(info.Thunk);

        // Default implementation.
        reqt.addRelativeAddressOrNull(info.DefaultImpl);

        // Add the associated type name to the list.
        if (entry.isAssociatedType()) {
          if (!AssociatedTypeNames.empty())
            AssociatedTypeNames += ' ';
          AssociatedTypeNames += entry.getAssociatedType()->getName().str();
        }

        reqt.finishAndAddTo(reqtsArray);
      }

      auto global =
        cast<llvm::GlobalVariable>(
          IGM.getAddrOfProtocolRequirementArray(Protocol,
                                                reqtsArray.finishAndCreateFuture()));
      global->setConstant(true);
      B.addRelativeOffset(IGM.Int32Ty, global);
      IGM.setTrueConstGlobal(global);
    }

    struct RequirementInfo {
      ProtocolRequirementFlags Flags;
      llvm::Constant *Thunk;
      llvm::Constant *DefaultImpl;
    };

    /// Build the information which will go into a ProtocolRequirement entry.
    RequirementInfo getRequirementInfo(const WitnessTableEntry &entry) {
      using Flags = ProtocolRequirementFlags;
      if (entry.isBase()) {
        assert(entry.isOutOfLineBase());
        auto flags = Flags(Flags::Kind::BaseProtocol);
        return { flags, nullptr, nullptr };
      }

      if (entry.isAssociatedType()) {
        auto flags = Flags(Flags::Kind::AssociatedTypeAccessFunction);
        return { flags, nullptr, nullptr };
      }

      if (entry.isAssociatedConformance()) {
        auto flags = Flags(Flags::Kind::AssociatedConformanceAccessFunction);
        return { flags, nullptr, nullptr };
      }

      assert(entry.isFunction());
      SILDeclRef func(entry.getFunction());

      // Look up the dispatch thunk if the protocol is resilient.
      llvm::Constant *thunk = nullptr;
      if (IGM.isResilient(Protocol, ResilienceExpansion::Minimal))
        thunk = IGM.getAddrOfDispatchThunk(func, NotForDefinition);

      // Classify the function.
      auto flags = getMethodDescriptorFlags<Flags>(func.getDecl());

      // Look for a default witness.
      llvm::Constant *defaultImpl = findDefaultWitness(func);

      return { flags, thunk, defaultImpl };
    }

    llvm::Constant *findDefaultWitness(SILDeclRef func) {
      if (!DefaultWitnesses) return nullptr;

      for (auto &entry : DefaultWitnesses->getEntries()) {
        if (!entry.isValid() || entry.getRequirement() != func)
          continue;
        return IGM.getAddrOfSILFunction(entry.getWitness(), NotForDefinition);
      }

      return nullptr;
    }

    void addSuperclass() {
      // FIXME: Implement.
      B.addRelativeAddressOrNull(nullptr);
    }

    void addAssociatedTypeNames() {
      llvm::Constant *global = nullptr;
      if (!AssociatedTypeNames.empty()) {
        global = IGM.getAddrOfGlobalString(AssociatedTypeNames,
                                           /*willBeRelativelyAddressed=*/true);
      }
      B.addRelativeAddressOrNull(global);
    }
  };
} // end anonymous namespace

/// Emit global structures associated with the given protocol. This comprises
/// the protocol descriptor, and for ObjC interop, references to the descriptor
/// that the ObjC runtime uses for uniquing.
void IRGenModule::emitProtocolDecl(ProtocolDecl *protocol) {
  // Emit remote reflection metadata for the protocol.
  emitFieldMetadataRecord(protocol);

  // If the protocol is Objective-C-compatible, go through the path that
  // produces an ObjC-compatible protocol_t.
  if (protocol->isObjC()) {
    // In JIT mode, we need to create protocol descriptors using the ObjC
    // runtime in JITted code.
    if (IRGen.Opts.UseJIT)
      return;
    
    // Native ObjC protocols are emitted on-demand in ObjC and uniqued by the
    // runtime; we don't need to try to emit a unique descriptor symbol for them.
    if (protocol->hasClangNode())
      return;
    
    getObjCProtocolGlobalVars(protocol);
    return;
  }

  SILDefaultWitnessTable *defaultWitnesses = nullptr;
  if (isResilient(protocol, ResilienceExpansion::Minimal))
    defaultWitnesses = getSILModule().lookUpDefaultWitnessTable(protocol);

  ConstantInitBuilder initBuilder(*this);
  auto init = initBuilder.beginStruct();
  ProtocolDescriptorBuilder builder(*this, protocol, init, defaultWitnesses);
  builder.layout();

  auto var = cast<llvm::GlobalVariable>(
          getAddrOfProtocolDescriptor(protocol, init.finishAndCreateFuture()));
  var->setConstant(true);
  disableAddressSanitizer(*this, var);

  // Note that we emitted this protocol.
  SwiftProtocols.push_back(protocol);

  // If the protocol is resilient, emit dispatch thunks.
  if (isResilient(protocol, ResilienceExpansion::Minimal)) {
    for (auto *member : protocol->getMembers()) {
      if (auto *funcDecl = dyn_cast<FuncDecl>(member)) {
        emitDispatchThunk(SILDeclRef(funcDecl));
      }
      if (auto *ctorDecl = dyn_cast<ConstructorDecl>(member)) {
        emitDispatchThunk(SILDeclRef(ctorDecl, SILDeclRef::Kind::Allocator));
      }
    }
  }
}

//===----------------------------------------------------------------------===//
// Generic requirements.
//===----------------------------------------------------------------------===//

/// Add a generic parameter reference to the given constant struct builder.
static void addGenericParamRef(IRGenModule &IGM, ConstantStructBuilder &B,
                               GenericSignature *sig, CanType type) {
  // type should be either a generic parameter or dependent member type
  // thereof.

  if (auto genericParam = dyn_cast<GenericTypeParamType>(type)) {
    // We can encode the ordinal of a direct type parameter reference
    // inline.
    auto ordinal = sig->getGenericParamOrdinal(genericParam);
    B.addInt32(ordinal << 1);
    return;
  }

  if (auto dmt = dyn_cast<DependentMemberType>(type)) {
    // We have to encode the associated type path out-of-line.
    auto assocTypeRecord = IGM.getAddrOfAssociatedTypeGenericParamRef(sig, dmt);

    B.addTaggedRelativeOffset(IGM.Int32Ty, assocTypeRecord, 1);
    return;
  }

  llvm_unreachable("not a generic parameter");
}

/// Add a generic requirement to the given constant struct builder.
static void addGenericRequirement(IRGenModule &IGM, ConstantStructBuilder &B,
                                  GenericRequirementsMetadata &metadata,
                                  GenericSignature *sig,
                                  GenericRequirementFlags flags,
                                  Type paramType,
                                  llvm::function_ref<void ()> addReference) {
  if (flags.hasKeyArgument())
    ++metadata.NumGenericKeyArguments;
  if (flags.hasExtraArgument())
    ++metadata.NumGenericExtraArguments;

  B.addInt(IGM.Int32Ty, flags.getIntValue());
  addGenericParamRef(IGM, B, sig, paramType->getCanonicalType());
  addReference();
}

GenericRequirementsMetadata irgen::addGenericRequirements(
                                   IRGenModule &IGM, ConstantStructBuilder &B,
                                   GenericSignature *sig,
                                   ArrayRef<Requirement> requirements) {
  assert(sig);
  GenericRequirementsMetadata metadata;
  for (auto &requirement : requirements) {
    ++metadata.NumRequirements;

    switch (auto kind = requirement.getKind()) {
    case RequirementKind::Layout:
      switch (auto layoutKind =
                requirement.getLayoutConstraint()->getKind()) {
      case LayoutConstraintKind::Class: {
        // Encode the class constraint.
        auto flags = GenericRequirementFlags(GenericRequirementKind::Layout,
                                             /*key argument*/ false,
                                             /*extra argument*/ false);
        addGenericRequirement(IGM, B, metadata, sig, flags,
                              requirement.getFirstType(),
         [&]{ B.addInt32((uint32_t)GenericRequirementLayoutKind::Class); });
        break;
      }
      default:
        // No other layout constraints are supported in source-level Swift
        // today.
        llvm_unreachable("shouldn't show up in ABI");
      }
      break;

    case RequirementKind::Conformance: {
      // ABI TODO: We also need a *key* argument that uniquely identifies
      // the conformance for conformance requirements as well.
      auto protocol = requirement.getSecondType()->castTo<ProtocolType>()
        ->getDecl();
      bool needsWitnessTable =
        Lowering::TypeConverter::protocolRequiresWitnessTable(protocol);
      auto flags = GenericRequirementFlags(GenericRequirementKind::Protocol,
                                           /*TODO key argument*/ false,
                                           needsWitnessTable);
      auto descriptorRef =
        IGM.getConstantReferenceForProtocolDescriptor(protocol);
      addGenericRequirement(IGM, B, metadata, sig, flags,
                            requirement.getFirstType(),
        [&]{ B.addRelativeAddress(descriptorRef); });
      break;
    }

    case RequirementKind::SameType:
    case RequirementKind::Superclass: {
      auto abiKind = kind == RequirementKind::SameType
        ? GenericRequirementKind::SameType
        : GenericRequirementKind::BaseClass;

      auto flags = GenericRequirementFlags(abiKind, false, false);
      auto typeName =
        IGM.getTypeRef(requirement.getSecondType()->getCanonicalType());

      addGenericRequirement(IGM, B, metadata, sig, flags,
                            requirement.getFirstType(),
        [&]{ B.addRelativeAddress(typeName); });

      // ABI TODO: Same type and superclass constraints also imply
      // "same conformance" constraints on any protocol requirements of
      // the constrained type, which we should emit.
      break;
    }
    }
  }

  return metadata;
}

//===----------------------------------------------------------------------===//
// Other metadata.
//===----------------------------------------------------------------------===//

llvm::Value *irgen::emitMetatypeInstanceType(IRGenFunction &IGF,
                                             llvm::Value *metatypeMetadata) {
  // The instance type field of MetatypeMetadata is immediately after
  // the isa field.
  return emitInvariantLoadFromMetadataAtIndex(IGF, metatypeMetadata, 1,
                                              IGF.IGM.TypeMetadataPtrTy);
}
