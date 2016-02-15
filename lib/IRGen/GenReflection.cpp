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

#include "swift/AST/Decl.h"
#include "swift/AST/IRGenOptions.h"

#include "ConstantBuilder.h"
#include "IRGenModule.h"

using namespace swift;
using namespace irgen;

class ReflectionMetadataBuilder : public ConstantBuilder<> {

  const uint32_t fieldRecordSize = 8;
  ArrayRef<const NominalTypeDecl *> NominalTypeDecls;

  void addFieldDecl(const ValueDecl *value) {
    auto type = value->getInterfaceType()->getCanonicalType();
    Mangle::Mangler mangler;
    mangler.setModuleContext(value->getModuleContext());
    mangler.mangleType(type, 0);
    auto mangledName = IGM.getAddrOfStringForTypeRef(mangler.finalize());
    addRelativeAddress(mangledName);

    if (IGM.Opts.StripReflectionNames) {
      addConstantInt32(0);
    } else {
      auto fieldName = IGM.getAddrOfFieldName(value->getNameStr());
      addRelativeAddress(fieldName);
    }
  }

  void addDecl(const NominalTypeDecl *decl) {
    auto type = decl->getDeclaredInterfaceType()->getCanonicalType();
    Mangle::Mangler mangler;
    mangler.setModuleContext(decl->getModuleContext());
    mangler.mangleType(type, 0);
    auto mangledName = IGM.getAddrOfStringForTypeRef(mangler.finalize());
    addRelativeAddress(mangledName);

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
  ReflectionMetadataBuilder(IRGenModule &IGM,
      ArrayRef<const NominalTypeDecl *> NominalTypeDecls)
    : ConstantBuilder(IGM), NominalTypeDecls(NominalTypeDecls) {}
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
    var->setSection(IGM.getReflectionMetadataSectionName());
    var->setAlignment(IGM.getPointerAlignment().getValue());

    auto replacer = llvm::ConstantExpr::getBitCast(var, IGM.Int8PtrTy);
    tempBase->replaceAllUsesWith(replacer);

    return var;
  }
};

StringRef IRGenModule::getReflectionMetadataSectionName() {
  switch (TargetInfo.OutputObjectFormat) {
    case llvm::Triple::MachO:
      return "__DATA, __swift3_reflect, regular, no_dead_strip";
      break;
    case llvm::Triple::ELF:
      return ".swift3_reflect";
      break;
    default:
      llvm_unreachable("Don't know how to emit field name table for "
                       "the selected object format.");
  }
}

StringRef IRGenModule::getReflectionStringsSectionName() {
  switch (TargetInfo.OutputObjectFormat) {
    case llvm::Triple::MachO:
      return "__DATA, __swift3_reflstr, coalesced, no_dead_strip";
      break;
    case llvm::Triple::ELF:
      return ".swift3_reflstr";
      break;
    default:
      llvm_unreachable("Don't know how to emit field metadata table for "
                       "the selected object format.");
  }
}

StringRef IRGenModule::getReflectionTypeRefSectionName() {
  switch (TargetInfo.OutputObjectFormat) {
    case llvm::Triple::MachO:
      return "__DATA, __swift3_typeref, coalesced, no_dead_strip";
      break;
    case llvm::Triple::ELF:
      return ".swift3_typeref";
      break;
    default:
      llvm_unreachable("Don't know how to emit field typeref table for "
                       "the selected object format.");
  }
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

llvm::Constant *IRGenModule::emitReflectionMetadataRecords() {
  if (Opts.StripReflectionMetadata)
    return nullptr;

  if (NominalTypeDecls.empty())
    return nullptr;

  ReflectionMetadataBuilder builder(*this, NominalTypeDecls);
  auto var = builder.emit();
  addUsedGlobal(var);
  return var;
}

