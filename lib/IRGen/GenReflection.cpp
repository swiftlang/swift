//===--- GenReflection.cpp - IR generation for nominal type reflection ----===//
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
//  This file implements IR generation of type metadata for struct/class
//  stored properties and enum cases for use with reflection.
//===----------------------------------------------------------------------===//

#include "swift/AST/ArchetypeBuilder.h"
#include "swift/AST/Decl.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/Reflection/Records.h"
#include "swift/SIL/SILModule.h"

#include "ConstantBuilder.h"
#include "IRGenModule.h"
#include "LoadableTypeInfo.h"

using namespace swift;
using namespace irgen;

class ReflectionMetadataBuilder : public ConstantBuilder<> {
protected:
  SmallPtrSetImpl<CanType> &BuiltinTypes;

  // Collect any builtin types referenced from this type.
  void addBuiltinTypeRefs(CanType type) {
    type.visit([&](Type t) {
      if (t->is<BuiltinType>())
        BuiltinTypes.insert(CanType(t));
    });
  }

  void addTypeRef(Module *ModuleContext, CanType type) {
    assert(type);
    Mangle::Mangler mangler(/*DWARFMangling*/false,
                            /*usePunyCode*/ true,
                            /*OptimizeProtocolNames*/ false);
    mangler.setModuleContext(ModuleContext);
    mangler.mangleType(type, 0);
    auto mangledName = IGM.getAddrOfStringForTypeRef(mangler.finalize());
    addRelativeAddress(mangledName);
  }

public:
  ReflectionMetadataBuilder(IRGenModule &IGM,
                            SmallPtrSetImpl<CanType> &BuiltinTypes)
    : ConstantBuilder(IGM), BuiltinTypes(BuiltinTypes) {}
};

class AssociatedTypeMetadataBuilder : public ReflectionMetadataBuilder {
  static const uint32_t AssociatedTypeRecordSize = 8;
  ArrayRef<const NominalTypeDecl *> NominalTypeDecls;
  ArrayRef<const ExtensionDecl *> ExtensionDecls;

  void addConformance(Module *ModuleContext,
                      CanType ConformingType,
                      const ProtocolConformance *Conformance) {
    SmallVector<std::pair<StringRef, CanType>, 2> AssociatedTypes;

    auto collectTypeWitness = [&](const AssociatedTypeDecl *AssocTy,
                                  const Substitution &Sub,
                                  const TypeDecl *TD) -> bool {

      auto Subst = ArchetypeBuilder::mapTypeOutOfContext(
        Conformance->getDeclContext(), Sub.getReplacement());

      AssociatedTypes.push_back({
        AssocTy->getNameStr(),
        Subst->getCanonicalType()
      });
      return false;
    };

    addTypeRef(ModuleContext, ConformingType);

    auto ProtoTy = Conformance->getProtocol()->getDeclaredType();
    addTypeRef(ModuleContext, ProtoTy->getCanonicalType());

    Conformance->forEachTypeWitness(/*resolver*/ nullptr, collectTypeWitness);

    addConstantInt32(AssociatedTypes.size());
    addConstantInt32(AssociatedTypeRecordSize);

    for (auto AssocTy : AssociatedTypes) {
      auto NameGlobal = IGM.getAddrOfStringForTypeRef(AssocTy.first);
      addRelativeAddress(NameGlobal);
      addBuiltinTypeRefs(AssocTy.second);
      addTypeRef(ModuleContext, AssocTy.second);
    }
  }

  void layout() {
    for (auto Decl : NominalTypeDecls) {
      PrettyStackTraceDecl DebugStack("emitting associated type metadata", Decl);
      for (auto Conformance : Decl->getAllConformances()) {
        if (Conformance->isIncomplete())
          continue;
        addConformance(Decl->getModuleContext(),
                       Decl->getDeclaredType()->getCanonicalType(),
                       Conformance);
      }
    }

    for (auto Ext : ExtensionDecls) {
      PrettyStackTraceDecl DebugStack("emitting associated type metadata", Ext);
      for (auto Conformance : Ext->getLocalConformances()) {
        auto Decl = Ext->getExtendedType()->getNominalOrBoundGenericNominal();
        addConformance(Ext->getDeclContext()->getParentModule(),
                       Decl->getDeclaredType()->getCanonicalType(),
                       Conformance);
      }
    }
  }

public:
  AssociatedTypeMetadataBuilder(IRGenModule &IGM,
    ArrayRef<const NominalTypeDecl *> NominalTypeDecls,
    ArrayRef<const ExtensionDecl *> ExtensionDecls,
    SmallPtrSetImpl<CanType> &BuiltinTypes)
    : ReflectionMetadataBuilder(IGM, BuiltinTypes),
      NominalTypeDecls(NominalTypeDecls),
      ExtensionDecls(ExtensionDecls) {}

  llvm::GlobalVariable *emit() {
    auto tempBase = std::unique_ptr<llvm::GlobalVariable>(
        new llvm::GlobalVariable(IGM.Int8Ty, /*isConstant*/ true,
                                 llvm::GlobalValue::PrivateLinkage));
    setRelativeAddressBase(tempBase.get());

    layout();
    auto init = getInit();
    if (!init)
      return nullptr;

    auto var = new llvm::GlobalVariable(*IGM.getModule(), init->getType(),
                                        /*isConstant*/ true,
                                        llvm::GlobalValue::PrivateLinkage,
                                        init,
                                        "\x01l__swift3_assocty_metadata");
    var->setSection(IGM.getAssociatedTypeMetadataSectionName());
    var->setAlignment(4);

    auto replacer = llvm::ConstantExpr::getBitCast(var, IGM.Int8PtrTy);
    tempBase->replaceAllUsesWith(replacer);
    
    return var;
  }
};

class FieldTypeMetadataBuilder : public ReflectionMetadataBuilder {
  const uint32_t fieldRecordSize = 12;
  ArrayRef<const NominalTypeDecl *> NominalTypeDecls;

  void addFieldDecl(const ValueDecl *value, CanType type) {
    swift::reflection::FieldRecordFlags Flags;
    Flags.setIsObjC(value->isObjC());

    addConstantInt32(Flags.getRawValue());

    if (!type) {
      addConstantInt32(0);
    } else {
      addTypeRef(value->getModuleContext(), type);
      addBuiltinTypeRefs(type);
    }

    if (IGM.Opts.StripReflectionNames) {
      addConstantInt32(0);
    } else {
      auto fieldName = IGM.getAddrOfFieldName(value->getNameStr());
      addRelativeAddress(fieldName);
    }
  }

  void addDecl(const NominalTypeDecl *decl) {
    using swift::reflection::FieldDescriptorKind;

    PrettyStackTraceDecl DebugStack("emitting field type metadata", decl);
    auto type = decl->getDeclaredType()->getCanonicalType();
    addTypeRef(decl->getModuleContext(), type);

    switch (decl->getKind()) {
    case DeclKind::Class:
    case DeclKind::Struct: {
      auto properties = decl->getStoredProperties();
      addConstantInt16(uint16_t(isa<StructDecl>(decl)
                                ? FieldDescriptorKind::Struct
                                : FieldDescriptorKind::Class));
      addConstantInt16(fieldRecordSize);
      addConstantInt32(std::distance(properties.begin(), properties.end()));
      for (auto property : properties)
        addFieldDecl(property,
                     property->getInterfaceType()
                       ->getCanonicalType());
      break;
    }
    case DeclKind::Enum: {
      auto enumDecl = cast<EnumDecl>(decl);
      auto cases = enumDecl->getAllElements();
      addConstantInt16(uint16_t(FieldDescriptorKind::Enum));
      addConstantInt16(fieldRecordSize);
      addConstantInt32(std::distance(cases.begin(), cases.end()));
      for (auto enumCase : cases) {
        if (enumCase->hasArgumentType()) {
          addFieldDecl(enumCase,
                       enumCase->getArgumentInterfaceType()
                         ->getCanonicalType());
        } else {
          addFieldDecl(enumCase, CanType());
        }
      }
      break;
    }
    default:
      llvm_unreachable("Not a nominal type");
      break;
    }
  }

  void layout() {
    for (auto decl : NominalTypeDecls) {
      addDecl(decl);
    }
  }

public:
  FieldTypeMetadataBuilder(IRGenModule &IGM,
                           ArrayRef<const NominalTypeDecl *> NominalTypeDecls,
                           SmallPtrSetImpl<CanType> &BuiltinTypes)
    : ReflectionMetadataBuilder(IGM, BuiltinTypes),
      NominalTypeDecls(NominalTypeDecls) {}

  llvm::GlobalVariable *emit() {

    auto tempBase = std::unique_ptr<llvm::GlobalVariable>(
        new llvm::GlobalVariable(IGM.Int8Ty, /*isConstant*/ true,
                                 llvm::GlobalValue::PrivateLinkage));
    setRelativeAddressBase(tempBase.get());

    layout();
    auto init = getInit();

    if (!init)
      return nullptr;

    auto var = new llvm::GlobalVariable(*IGM.getModule(), init->getType(),
                                        /*isConstant*/ true,
                                        llvm::GlobalValue::PrivateLinkage,
                                        init,
                                        "\x01l__swift3_reflection_metadata");
    var->setSection(IGM.getFieldTypeMetadataSectionName());
    var->setAlignment(4);

    auto replacer = llvm::ConstantExpr::getBitCast(var, IGM.Int8PtrTy);
    tempBase->replaceAllUsesWith(replacer);

    return var;
  }
};

class BuiltinTypeMetadataBuilder : public ReflectionMetadataBuilder {
  void addBuiltinType(CanType builtinType) {
    addTypeRef(builtinType->getASTContext().TheBuiltinModule, builtinType);

    auto &ti = cast<LoadableTypeInfo>(IGM.getTypeInfoForUnlowered(builtinType));
    addConstantInt32(ti.getFixedSize().getValue());
    addConstantInt32(ti.getFixedAlignment().getValue());
    addConstantInt32(ti.getFixedStride().getValue());
    addConstantInt32(ti.getFixedExtraInhabitantCount(IGM));
  }

  void layout() {
    for (auto builtinType : BuiltinTypes) {
      addBuiltinType(builtinType);
    }
  }

public:
  BuiltinTypeMetadataBuilder(IRGenModule &IGM,
                             SmallPtrSetImpl<CanType> &BuiltinTypes)
    : ReflectionMetadataBuilder(IGM, BuiltinTypes) {}

  llvm::GlobalVariable *emit() {

    auto tempBase = std::unique_ptr<llvm::GlobalVariable>(
        new llvm::GlobalVariable(IGM.Int8Ty, /*isConstant*/ true,
                                 llvm::GlobalValue::PrivateLinkage));
    setRelativeAddressBase(tempBase.get());

    layout();
    auto init = getInit();

    if (!init)
      return nullptr;

    auto var = new llvm::GlobalVariable(*IGM.getModule(), init->getType(),
                                        /*isConstant*/ true,
                                        llvm::GlobalValue::PrivateLinkage,
                                        init,
                                        "\x01l__swift3_builtin_metadata");
    var->setSection(IGM.getBuiltinTypeMetadataSectionName());
    var->setAlignment(IGM.getPointerAlignment().getValue());

    auto replacer = llvm::ConstantExpr::getBitCast(var, IGM.Int8PtrTy);
    tempBase->replaceAllUsesWith(replacer);

    return var;
  }
};

static std::string getReflectionSectionName(IRGenModule &IGM,
                                            std::string Base) {
  SmallString<50> SectionName;
  llvm::raw_svector_ostream OS(SectionName);
  switch (IGM.TargetInfo.OutputObjectFormat) {
    case llvm::Triple::MachO:
      assert(Base.size() <= 7
             && "Mach-O section name length must be <= 16 characters");
      OS << "__DATA, __swift3_" << Base << ", regular, no_dead_strip";
      break;
    case llvm::Triple::ELF:
      OS << ".swift3_" << Base;
      break;
    default:
      llvm_unreachable("Don't know how to emit field name table for "
                       "the selected object format.");
  }
  return OS.str();
}

std::string IRGenModule::getFieldTypeMetadataSectionName() {
  return getReflectionSectionName(*this, "fieldmd");
}

std::string IRGenModule::getBuiltinTypeMetadataSectionName() {
  return getReflectionSectionName(*this, "builtin");
}

std::string IRGenModule::getAssociatedTypeMetadataSectionName() {
  return getReflectionSectionName(*this, "assocty");
}

std::string IRGenModule::getReflectionStringsSectionName() {
  return getReflectionSectionName(*this, "reflstr");
}

std::string IRGenModule::getReflectionTypeRefSectionName() {
  return getReflectionSectionName(*this, "typeref");
}

llvm::Constant *IRGenModule::getAddrOfFieldName(StringRef Name) {
  auto &entry = FieldNames[Name];
  if (entry.second)
    return entry.second;

  entry = createStringConstant(Name, /*willBeRelativelyAddressed*/ true,
                               getReflectionStringsSectionName());
  return entry.second;
}

llvm::Constant *IRGenModule::getAddrOfStringForTypeRef(StringRef Str) {
  auto &entry = StringsForTypeRef[Str];
  if (entry.second)
    return entry.second;

  entry = createStringConstant(Str, /*willBeRelativelyAddressed*/ true,
                               getReflectionTypeRefSectionName());
  return entry.second;
}

void IRGenModule::emitReflectionMetadataRecords() {
  auto DontHaveDecls = NominalTypeDecls.empty() && ExtensionDecls.empty();
  if (Opts.StripReflectionMetadata || DontHaveDecls)
    return;

  // We collect all referenced builtin types and emit records for them.
  // In practice only the standard library should directly reference
  // builtin types.
  //
  // FIXME: This metadata should be in the runtime instead.
  SmallPtrSet<CanType, 4> BuiltinTypes;

  {
    FieldTypeMetadataBuilder builder(*this, NominalTypeDecls, BuiltinTypes);
    auto var = builder.emit();
    if (var)
      addUsedGlobal(var);
  }

  {
    AssociatedTypeMetadataBuilder builder(*this,
                                          NominalTypeDecls,
                                          ExtensionDecls,
                                          BuiltinTypes);
    auto var = builder.emit();
    if (var)
      addUsedGlobal(var);
  }

  {
    BuiltinTypeMetadataBuilder builder(*this, BuiltinTypes);
    auto var = builder.emit();
    if (var)
      addUsedGlobal(var);
  }
}
