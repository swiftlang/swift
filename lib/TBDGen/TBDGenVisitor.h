//===--- TBDGenVisitor.h - AST Visitor for TBD generation -----------------===//
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
//  This file defines the visitor that finds all symbols in a swift AST.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_TBDGEN_TBDGENVISITOR_H
#define SWIFT_TBDGEN_TBDGENVISITOR_H

#include "swift/AST/ASTMangler.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/Basic/LLVM.h"
#include "swift/IRGen/Linking.h"
#include "swift/SIL/FormalLinkage.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILWitnessTable.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/ADT/Triple.h"

using namespace swift::irgen;
using StringSet = llvm::StringSet<>;

namespace swift {

struct TBDGenOptions;

namespace tbdgen {

class TBDGenVisitor : public ASTVisitor<TBDGenVisitor> {
public:
  StringSet &Symbols;
  const llvm::Triple &Triple;
  const UniversalLinkageInfo &UniversalLinkInfo;
  ModuleDecl *SwiftModule;
  TBDGenOptions &Opts;

private:
  bool FileHasEntryPoint = false;

  void addSymbol(StringRef name) {
    auto isNewValue = Symbols.insert(name).second;
    (void)isNewValue;
    assert(isNewValue && "already inserted");
  }

  void addSymbol(SILDeclRef declRef);

  void addSymbol(LinkEntity entity) {
    auto linkage =
        LinkInfo::get(UniversalLinkInfo, SwiftModule, entity, ForDefinition);

    auto externallyVisible =
        llvm::GlobalValue::isExternalLinkage(linkage.getLinkage()) &&
        linkage.getVisibility() != llvm::GlobalValue::HiddenVisibility;

    if (externallyVisible)
      addSymbol(linkage.getName());
  }

  void addConformances(DeclContext *DC);

  void addDispatchThunk(SILDeclRef declRef);

public:
  TBDGenVisitor(StringSet &symbols, const llvm::Triple &triple,
                const UniversalLinkageInfo &universalLinkInfo,
                ModuleDecl *swiftModule, TBDGenOptions &opts)
      : Symbols(symbols), Triple(triple), UniversalLinkInfo(universalLinkInfo),
        SwiftModule(swiftModule), Opts(opts) {}

  void setFileHasEntryPoint(bool hasEntryPoint) {
    FileHasEntryPoint = hasEntryPoint;

    if (hasEntryPoint)
      addSymbol("main");
  }

  /// \brief Adds the global symbols associated with the first file.
  void addFirstFileSymbols();

  void visitPatternBindingDecl(PatternBindingDecl *PBD);

  void visitAbstractFunctionDecl(AbstractFunctionDecl *AFD);

  void visitAccessorDecl(AccessorDecl *AD);

  void visitNominalTypeDecl(NominalTypeDecl *NTD);

  void visitClassDecl(ClassDecl *CD);

  void visitConstructorDecl(ConstructorDecl *CD);

  void visitDestructorDecl(DestructorDecl *DD);

  void visitExtensionDecl(ExtensionDecl *ED);

  void visitProtocolDecl(ProtocolDecl *PD);

  void visitAbstractStorageDecl(AbstractStorageDecl *ASD);

  void visitVarDecl(VarDecl *VD);

  void visitEnumDecl(EnumDecl *ED);

  void visitDecl(Decl *D) {}
};
} // end namespace tbdgen
} // end namespace swift

#endif
