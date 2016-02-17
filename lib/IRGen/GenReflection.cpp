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
#include "swift/AST/ProtocolConformance.h"

#include "ConstantBuilder.h"
#include "IRGenModule.h"

using namespace swift;
using namespace irgen;

class ReflectionMetadataBuilder : public ConstantBuilder<> {
protected:
  void addTypeRef(Module *ModuleContext, CanType type) {
    Mangle::Mangler mangler;
    mangler.setModuleContext(ModuleContext);
    mangler.mangleType(type, 0);
    auto mangledName = IGM.getAddrOfStringForTypeRef(mangler.finalize());
    addRelativeAddress(mangledName);
  }

public:
  ReflectionMetadataBuilder(IRGenModule &IGM) : ConstantBuilder(IGM) {}
};

class AssociatedTypeMetadataBuilder : public ReflectionMetadataBuilder {
  static const uint32_t AssociatedTypeRecordSize = 8;
  ArrayRef<const NominalTypeDecl *> NominalTypeDecls;

  void addDecl(const NominalTypeDecl *Decl) {
    for (auto Conformance : Decl->getAllConformances()) {
      SmallVector<std::pair<StringRef, CanType>, 2> AssociatedTypes;

      auto collectTypeWitness = [&](const AssociatedTypeDecl *AssocTy,
                                    const Substitution &Sub,
                                    const TypeDecl *TD) -> bool {

        Type Subst = Sub.getReplacement();
        if (auto InterfaceTy = ArchetypeBuilder::mapTypeOutOfContext(
            Conformance->getDeclContext(), Subst)) {
          Subst = InterfaceTy;
        }

        AssociatedTypes.push_back({
          AssocTy->getNameStr(),
          Subst->getCanonicalType()
        });
        return false;
      };

      auto ModuleContext = Decl->getModuleContext();

      auto ConformingTy = Conformance->getInterfaceType();
      addTypeRef(ModuleContext, ConformingTy->getCanonicalType());

      auto ProtoTy = Conformance->getProtocol()->getInterfaceType();
      addTypeRef(ModuleContext, ProtoTy->getCanonicalType());

      Conformance->forEachTypeWitness(/*resolver*/ nullptr, collectTypeWitness);

      addConstantInt32(AssociatedTypes.size());
      addConstantInt32(AssociatedTypeRecordSize);

      for (auto AssocTy : AssociatedTypes) {
        auto NameGlobal = IGM.getAddrOfStringForTypeRef(AssocTy.first);
        addRelativeAddress(NameGlobal);
        addTypeRef(ModuleContext, AssocTy.second);
      }
    }
  }

public:
  AssociatedTypeMetadataBuilder(IRGenModule &IGM,
      ArrayRef<const NominalTypeDecl *> NominalTypeDecls)
    : ReflectionMetadataBuilder(IGM), NominalTypeDecls(NominalTypeDecls) {}

  void layout() {
    for (auto decl : NominalTypeDecls)
      addDecl(decl);
  }

  llvm::GlobalVariable *emit() {
    auto tempBase = std::unique_ptr<llvm::GlobalVariable>(
        new llvm::GlobalVariable(IGM.Int8Ty, /*isConstant*/ true,
        llvm::GlobalValue::PrivateLinkage));
    setRelativeAddressBase(tempBase.get());

    layout();
    auto init = getInit();
    auto var = new llvm::GlobalVariable(*IGM.getModule(), init->getType(),
                                        /*isConstant*/ true,
                                        llvm::GlobalValue::PrivateLinkage,
                                        init,
                                        "\x01l__swift3_assocty_metadata");
    var->setSection(IGM.getAssociatedTypeMetadataSectionName());
    var->setAlignment(IGM.getPointerAlignment().getValue());

    auto replacer = llvm::ConstantExpr::getBitCast(var, IGM.Int8PtrTy);
    tempBase->replaceAllUsesWith(replacer);
    
    return var;
  }
};

class FieldTypeMetadataBuilder : public ReflectionMetadataBuilder {

  const uint32_t fieldRecordSize = 8;
  ArrayRef<const NominalTypeDecl *> NominalTypeDecls;

  void addFieldDecl(const ValueDecl *value) {
    auto type = value->getInterfaceType()->getCanonicalType();
    addTypeRef(value->getModuleContext(), type);

    if (IGM.Opts.StripReflectionNames) {
      addConstantInt32(0);
    } else {
      auto fieldName = IGM.getAddrOfFieldName(value->getNameStr());
      addRelativeAddress(fieldName);
    }
  }

  void addDecl(const NominalTypeDecl *decl) {
    auto type = decl->getDeclaredInterfaceType()->getCanonicalType();
    addTypeRef(decl->getModuleContext(), type);

    switch (decl->getKind()) {
    case DeclKind::Class:
    case DeclKind::Struct: {
      auto properties = decl->getStoredProperties();
      addConstantInt32(std::distance(properties.begin(), properties.end()));
      addConstantInt32(fieldRecordSize);
      for (auto property : properties)
        addFieldDecl(property);
      break;
    }
    case DeclKind::Enum: {
      auto enumDecl = cast<EnumDecl>(decl);
      auto cases = enumDecl->getAllElements();
      addConstantInt32(std::distance(cases.begin(), cases.end()));
      addConstantInt32(fieldRecordSize);
      for (auto enumCase : cases)
        addFieldDecl(enumCase);
      break;
    }
    default:
      llvm_unreachable("Not a nominal type");
      break;
    }
  }

public:
  FieldTypeMetadataBuilder(IRGenModule &IGM,
      ArrayRef<const NominalTypeDecl *> NominalTypeDecls)
    : ReflectionMetadataBuilder(IGM), NominalTypeDecls(NominalTypeDecls) {}
  void layout() {
    for (auto decl : NominalTypeDecls) {
      addDecl(decl);
    }
  }

  llvm::GlobalVariable *emit() {
    auto tempBase = std::unique_ptr<llvm::GlobalVariable>(
        new llvm::GlobalVariable(IGM.Int8Ty, /*isConstant*/ true,
                                 llvm::GlobalValue::PrivateLinkage));
    setRelativeAddressBase(tempBase.get());

    layout();
    auto init = getInit();
    auto var = new llvm::GlobalVariable(*IGM.getModule(), init->getType(),
                                        /*isConstant*/ true,
                                        llvm::GlobalValue::PrivateLinkage,
                                        init,
                                        "\x01l__swift3_reflection_metadata");
    var->setSection(IGM.getFieldTypeMetadataSectionName());
    var->setAlignment(IGM.getPointerAlignment().getValue());

    auto replacer = llvm::ConstantExpr::getBitCast(var, IGM.Int8PtrTy);
    tempBase->replaceAllUsesWith(replacer);

    return var;
  }
};

static std::string getReflectionSectionName(IRGenModule &IGM,
                                            std::string Base) {
  assert(Base.size() <= 7
         && "Mach-O section name length must be <= 16 characters");
  SmallString<50> SectionName;
  llvm::raw_svector_ostream OS(SectionName);
  switch (IGM.TargetInfo.OutputObjectFormat) {
    case llvm::Triple::MachO:
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

llvm::Constant *IRGenModule::emitFieldTypeMetadataRecords() {
  if (Opts.StripReflectionMetadata)
    return nullptr;

  if (NominalTypeDecls.empty())
    return nullptr;

  FieldTypeMetadataBuilder builder(*this, NominalTypeDecls);
  auto var = builder.emit();
  addUsedGlobal(var);
  return var;
}

llvm::Constant *IRGenModule::emitAssociatedTypeMetadataRecords() {
  if (Opts.StripReflectionMetadata)
    return nullptr;

  if (NominalTypeDecls.empty())
    return nullptr;

  AssociatedTypeMetadataBuilder builder(*this, NominalTypeDecls);
  auto var = builder.emit();
  addUsedGlobal(var);
  return var;
}
