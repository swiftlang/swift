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
#include "swift/IRGen/LinkEntity.h"
#include "swift/SIL/FormalLinkage.h"
#include "swift/SIL/SILDeclRef.h"
#include "llvm/ADT/StringSet.h"

using namespace swift;
using StringSet = llvm::StringSet<>;

static bool isPrivateDecl(ValueDecl *VD) {
  return getDeclLinkage(VD) != FormalLinkage::PublicUnique;
}

namespace {
class TBDGenVisitor : public ASTVisitor<TBDGenVisitor> {
  StringSet &Symbols;

  void addSymbol(StringRef name) {
    auto isNewValue = Symbols.insert(name).second;
    (void)isNewValue;
    assert(isNewValue && "already inserted");
  }

  void visitValueTypeDecl(NominalTypeDecl *NTD) {
    assert(isa<StructDecl>(NTD) || isa<EnumDecl>(NTD));
    if (isPrivateDecl(NTD))
      return;

    auto declaredType = NTD->getDeclaredType()->getCanonicalType();

    auto vwt = irgen::LinkEntity::forValueWitnessTable(declaredType);
    addSymbol(vwt.mangleAsString());

    visitNominalTypeDecl(NTD);
  }

public:
  TBDGenVisitor(StringSet &symbols) : Symbols(symbols) {}

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
  void visitValueDecl(ValueDecl *VD) {
    if (isPrivateDecl(VD))
      return;

    auto declRef = SILDeclRef(VD);
    addSymbol(declRef.mangle());

    visitMembers(VD);
  }
  void visitTypeAliasDecl(TypeAliasDecl *TAD) {
    // any information here is encoded elsewhere
  }
  void visitNominalTypeDecl(NominalTypeDecl *NTD) {
    auto declaredType = NTD->getDeclaredType()->getCanonicalType();

    auto ntDescriptor = irgen::LinkEntity::forNominalTypeDescriptor(NTD);
    addSymbol(ntDescriptor.mangleAsString());

    auto tmd = irgen::LinkEntity::forTypeMetadata(
        declaredType, irgen::TypeMetadataAddress::AddressPoint,
        /*isPattern=*/false);
    addSymbol(tmd.mangleAsString());
    auto tmda = irgen::LinkEntity::forTypeMetadataAccessFunction(declaredType);
    addSymbol(tmda.mangleAsString());

    if (isPrivateDecl(NTD))
      return;

    visitMembers(NTD);
  }
  void visitClassDecl(ClassDecl *CD) {
    if (isPrivateDecl(CD))
      return;

    auto declaredType = CD->getDeclaredType()->getCanonicalType();

    auto tmlcv =
        irgen::LinkEntity::forTypeMetadataLazyCacheVariable(declaredType);
    addSymbol(tmlcv.mangleAsString());

    visitNominalTypeDecl(CD);
  }

  void visitStructDecl(StructDecl *SD) { visitValueTypeDecl(SD); }
  void visitEnumDecl(EnumDecl *ED) { visitValueTypeDecl(ED); }
  void visitProtocolDecl(ProtocolDecl *PD) {
    if (isPrivateDecl(PD))
      return;

    auto pDescriptor = irgen::LinkEntity::forProtocolDescriptor(PD);
    addSymbol(pDescriptor.mangleAsString());

    // There's no relevant information about members of a protocol at individual
    // protocols, each conforming type has to handle them individually.
  }

  void visitVarDecl(VarDecl *VD);

  void visitDecl(Decl *D) { visitMembers(D); }
};
}

void TBDGenVisitor::visitVarDecl(VarDecl *VD) {
  if (isPrivateDecl(VD))
    return;

  // statically/globally stored variables have some special handling.
  if (VD->hasStorage() &&
      (VD->isStatic() || VD->getDeclContext()->isModuleScopeContext())) {
    // The actual variable has a symbol, even when private.
    Mangle::ASTMangler mangler;
    addSymbol(mangler.mangleEntity(VD, false));

    auto accessor = SILDeclRef(VD, SILDeclRef::Kind::GlobalAccessor);
    addSymbol(accessor.mangle());
  }

  visitMembers(VD);
}

void swift::enumeratePublicSymbols(FileUnit *file, StringSet &symbols) {
  SmallVector<Decl *, 16> decls;
  file->getTopLevelDecls(decls);

  TBDGenVisitor visitor(symbols);
  for (auto d : decls)
    visitor.visit(d);
}
