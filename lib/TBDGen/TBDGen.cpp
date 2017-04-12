//===--- TBDGen.cpp - Swift TBD Generation --------------------------------===//
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
//  This file implements the entrypoints into TBD file generation.
//
//===----------------------------------------------------------------------===//

#include "swift/TBDGen/TBDGen.h"

#include "swift/AST/ASTMangler.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Module.h"
#include "swift/Basic/LLVM.h"
#include "swift/IRGen/Linking.h"
#include "swift/SIL/FormalLinkage.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/ADT/StringSet.h"

using namespace swift;
using namespace swift::irgen;
using StringSet = llvm::StringSet<>;

static bool isPrivateDecl(ValueDecl *VD) {
  return getDeclLinkage(VD) != FormalLinkage::PublicUnique;
}

namespace {
class TBDGenVisitor : public ASTVisitor<TBDGenVisitor> {
  StringSet &Symbols;
  const UniversalLinkageInfo &UniversalLinkInfo;
  ModuleDecl *SwiftModule;

  void addSymbol(StringRef name) {
    auto isNewValue = Symbols.insert(name).second;
    (void)isNewValue;
    assert(isNewValue && "already inserted");
  }

  void addSymbol(LinkEntity entity) {
    auto linkage =
        LinkInfo::get(UniversalLinkInfo, SwiftModule, entity, ForDefinition);

    auto externallyVisible =
        llvm::GlobalValue::isExternalLinkage(linkage.getLinkage()) &&
        linkage.getVisibility() != llvm::GlobalValue::HiddenVisibility;

    if (externallyVisible)
      addSymbol(linkage.getName());
  }

public:
  TBDGenVisitor(StringSet &symbols,
                const UniversalLinkageInfo &universalLinkInfo,
                ModuleDecl *swiftModule)
      : Symbols(symbols), UniversalLinkInfo(universalLinkInfo),
        SwiftModule(swiftModule) {}

  void visitMembers(Decl *D) {
    SmallVector<Decl *, 4> members;
    auto addMembers = [&](DeclRange range) {
      for (auto member : range)
        members.push_back(member);
    };
    if (auto ED = dyn_cast<ExtensionDecl>(D))
      addMembers(ED->getMembers());
    else if (auto NTD = dyn_cast<NominalTypeDecl>(D))
      addMembers(NTD->getMembers());

    for (auto member : members) {
      ASTVisitor::visit(member);
    }
  }

  void visitPatternBindingDecl(PatternBindingDecl *PBD);

  void visitValueDecl(ValueDecl *VD);

  void visitTypeAliasDecl(TypeAliasDecl *TAD) {
    // any information here is encoded elsewhere
  }

  void visitNominalTypeDecl(NominalTypeDecl *NTD);

  void visitClassDecl(ClassDecl *CD);

  void visitProtocolDecl(ProtocolDecl *PD) {
    addSymbol(LinkEntity::forProtocolDescriptor(PD));

#ifndef NDEBUG
    // There's no (currently) relevant information about members of a protocol
    // at individual protocols, each conforming type has to handle them
    // individually. Let's assert this fact:
    for (auto *member : PD->getMembers()) {
      auto isExpectedKind =
          isa<TypeAliasDecl>(member) || isa<AssociatedTypeDecl>(member) ||
          isa<AbstractStorageDecl>(member) || isa<PatternBindingDecl>(member) ||
          isa<AbstractFunctionDecl>(member);
      assert(isExpectedKind &&
             "unexpected member of protocol during TBD generation");
    }
#endif
  }

  void visitVarDecl(VarDecl *VD);

  void visitDecl(Decl *D) { visitMembers(D); }
};
}

static bool isGlobalOrStaticVar(VarDecl *VD) {
  return VD->isStatic() || VD->getDeclContext()->isModuleScopeContext();
}

void TBDGenVisitor::visitPatternBindingDecl(PatternBindingDecl *PBD) {
  for (auto &entry : PBD->getPatternList()) {
    auto *var = entry.getAnchoringVarDecl();
    if (isPrivateDecl(var))
      return;

    // Non-global variables might have an explicit initializer symbol.
    if (entry.getInit() && !isGlobalOrStaticVar(var)) {
      auto declRef =
          SILDeclRef(var, SILDeclRef::Kind::StoredPropertyInitializer);
      addSymbol(declRef.mangle());
    }
  }
}

void TBDGenVisitor::visitValueDecl(ValueDecl *VD) {
  if (isPrivateDecl(VD))
    return;

  auto declRef = SILDeclRef(VD);

  // Transparent symbols don't exist, even in public.
  // FIXME: this should really be "is SIL only".
  if (declRef.isTransparent())
    return;

  addSymbol(declRef.mangle());

  visitMembers(VD);
}

void TBDGenVisitor::visitVarDecl(VarDecl *VD) {
  if (isPrivateDecl(VD))
    return;

  // statically/globally stored variables have some special handling.
  if (VD->hasStorage() && isGlobalOrStaticVar(VD)) {
    // The actual variable has a symbol.
    Mangle::ASTMangler mangler;
    addSymbol(mangler.mangleEntity(VD, false));

    auto accessor = SILDeclRef(VD, SILDeclRef::Kind::GlobalAccessor);
    addSymbol(accessor.mangle());
  }

  visitMembers(VD);
}

void TBDGenVisitor::visitNominalTypeDecl(NominalTypeDecl *NTD) {
  auto declaredType = NTD->getDeclaredType()->getCanonicalType();

  addSymbol(LinkEntity::forNominalTypeDescriptor(NTD));

  addSymbol(LinkEntity::forTypeMetadata(declaredType,
                                        TypeMetadataAddress::AddressPoint,
                                        /*isPattern=*/false));
  addSymbol(LinkEntity::forTypeMetadataAccessFunction(declaredType));

  // There are symbols associated with any protocols this type conforms to.
  for (auto conformance : NTD->getLocalConformances()) {
    auto needsWTable = Lowering::TypeConverter::protocolRequiresWitnessTable(
        conformance->getProtocol());
    if (!needsWTable)
      continue;

    addSymbol(LinkEntity::forDirectProtocolWitnessTable(conformance));
    addSymbol(LinkEntity::forProtocolWitnessTableAccessFunction(conformance));
  }

  visitMembers(NTD);
}

template <typename T> bool isaAnd(Decl *D, llvm::function_ref<bool(T *)> f) {
  if (auto *x = dyn_cast_or_null<T>(D))
    return f(x);

  return false;
}

void TBDGenVisitor::visitClassDecl(ClassDecl *CD) {
  auto &ctxt = CD->getASTContext();

  if (ctxt.LangOpts.EnableObjCInterop) {
    addSymbol(LinkEntity::forSwiftMetaclassStub(CD));
  }

  // Some members of classes get extra handling, beyond members of struct/enums,
  // so let's walk over them manually.
  for (auto *member : CD->getMembers()) {
    auto value = dyn_cast<ValueDecl>(member);
    if (!value)
      continue;

    auto hasWitnessTableOffset =
        isa<ConstructorDecl>(value) ||
        isaAnd<FuncDecl>(value, [](FuncDecl *FD) {
          auto ASD = FD->getAccessorStorageDecl();
          // Static functions (including getters/setters of static variables)
          // and getters of lets do not get WTOs.
          auto forLet =
              isaAnd<VarDecl>(ASD, [](VarDecl *VD) { return VD->isLet(); });
          return !(FD->isStatic() || forLet);
        });
    if (hasWitnessTableOffset) {
      addSymbol(LinkEntity::forWitnessTableOffset(value));
    }
  }

  visitNominalTypeDecl(CD);
}

void swift::enumeratePublicSymbols(FileUnit *file, StringSet &symbols,
                                   bool hasMultipleIRGenThreads,
                                   bool isWholeModule) {
  UniversalLinkageInfo linkInfo(file->getASTContext().LangOpts.Target,
                                hasMultipleIRGenThreads, isWholeModule);

  SmallVector<Decl *, 16> decls;
  file->getTopLevelDecls(decls);

  TBDGenVisitor visitor(symbols, linkInfo, file->getParentModule());
  for (auto d : decls)
    visitor.visit(d);
}
