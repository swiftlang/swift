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
#include "swift/AST/FileUnit.h"
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
#include "llvm/TextAPI/MachO/InterfaceFile.h"

using namespace swift::irgen;
using StringSet = llvm::StringSet<>;

namespace llvm {
class DataLayout;
}

namespace swift {

struct TBDGenOptions;

namespace tbdgen {

enum class LinkerPlatformId: uint8_t {
#define LD_PLATFORM(Name, Id) Name = Id,
#include "ldPlatformKinds.def"
};

struct InstallNameStore {
  // The default install name to use when no specific install name is specified.
  std::string InstallName;
  // The install name specific to the platform id. This takes precedence over
  // the default install name.
  std::map<uint8_t, std::string> PlatformInstallName;
  StringRef getInstallName(LinkerPlatformId Id) const;
  void remark(ASTContext &Ctx, StringRef ModuleName) const;
};

class TBDGenVisitor : public ASTVisitor<TBDGenVisitor> {
public:
  llvm::MachO::InterfaceFile &Symbols;
  llvm::MachO::TargetList Targets;
  StringSet *StringSymbols;
  const llvm::DataLayout &DataLayout;

  const UniversalLinkageInfo &UniversalLinkInfo;
  ModuleDecl *SwiftModule;
  const TBDGenOptions &Opts;

  /// A set of original function and derivative configuration pairs for which
  /// derivative symbols have been emitted.
  ///
  /// Used to deduplicate derivative symbol emission for `@differentiable` and
  /// `@derivative` attributes.
  llvm::DenseSet<std::pair<AbstractFunctionDecl *, AutoDiffConfig>>
      AddedDerivatives;

private:
  std::vector<Decl*> DeclStack;
  std::unique_ptr<std::map<std::string, InstallNameStore>>
    previousInstallNameMap;
  std::unique_ptr<std::map<std::string, InstallNameStore>>
    parsePreviousModuleInstallNameMap();
  void addSymbolInternal(StringRef name, llvm::MachO::SymbolKind kind,
                         bool isLinkerDirective = false);
  void addLinkerDirectiveSymbolsLdHide(StringRef name, llvm::MachO::SymbolKind kind);
  void addLinkerDirectiveSymbolsLdPrevious(StringRef name, llvm::MachO::SymbolKind kind);
  void addSymbol(StringRef name, llvm::MachO::SymbolKind kind =
                                     llvm::MachO::SymbolKind::GlobalSymbol);

  void addSymbol(SILDeclRef declRef);

  void addSymbol(LinkEntity entity);

  void addConformances(DeclContext *DC);

  void addDispatchThunk(SILDeclRef declRef);

  void addMethodDescriptor(SILDeclRef declRef);

  void addProtocolRequirementsBaseDescriptor(ProtocolDecl *proto);
  void addAssociatedTypeDescriptor(AssociatedTypeDecl *assocType);
  void addAssociatedConformanceDescriptor(AssociatedConformance conformance);
  void addBaseConformanceDescriptor(BaseConformance conformance);

  /// Adds the symbol for the linear map function of the given kind associated
  /// with the given original function and derivative function configuration.
  void addAutoDiffLinearMapFunction(AbstractFunctionDecl *original,
                                    AutoDiffConfig config,
                                    AutoDiffLinearMapKind kind);

  /// Adds the symbol for the autodiff function of the given kind associated
  /// with the given original function, parameter indices, and derivative
  /// generic signature.
  void
  addAutoDiffDerivativeFunction(AbstractFunctionDecl *original,
                                IndexSubset *parameterIndices,
                                GenericSignature derivativeGenericSignature,
                                AutoDiffDerivativeFunctionKind kind);

  /// Adds the symbol for the differentiability witness associated with the
  /// given original function, AST parameter indices, result indices, and
  /// derivative generic signature.
  void addDifferentiabilityWitness(AbstractFunctionDecl *original,
                                   IndexSubset *astParameterIndices,
                                   IndexSubset *resultIndices,
                                   GenericSignature derivativeGenericSignature);

  /// Adds symbols associated with the given original function and
  /// derivative function configuration.
  void addDerivativeConfiguration(AbstractFunctionDecl *original,
                                  AutoDiffConfig config);

public:
  TBDGenVisitor(llvm::MachO::InterfaceFile &symbols,
                llvm::MachO::TargetList targets, StringSet *stringSymbols,
                const llvm::DataLayout &dataLayout,
                const UniversalLinkageInfo &universalLinkInfo,
                ModuleDecl *swiftModule, const TBDGenOptions &opts)
      : Symbols(symbols), Targets(targets), StringSymbols(stringSymbols),
        DataLayout(dataLayout), UniversalLinkInfo(universalLinkInfo),
        SwiftModule(swiftModule), Opts(opts),
        previousInstallNameMap(parsePreviousModuleInstallNameMap())  {}
  ~TBDGenVisitor() { assert(DeclStack.empty()); }
  void addMainIfNecessary(FileUnit *file) {
    // HACK: 'main' is a special symbol that's always emitted in SILGen if
    //       the file has an entry point. Since it doesn't show up in the
    //       module until SILGen, we need to explicitly add it here.
    //
    // Make sure to only add the main symbol for the module that we're emitting
    // TBD for, and not for any statically linked libraries.
    if (file->hasEntryPoint() && file->getParentModule() == SwiftModule)
      addSymbol("main");
  }

  /// Adds the global symbols associated with the first file.
  void addFirstFileSymbols();

  void visitDefaultArguments(ValueDecl *VD, ParameterList *PL);

  void visitAbstractFunctionDecl(AbstractFunctionDecl *AFD);

  void visitAccessorDecl(AccessorDecl *AD);

  void visitNominalTypeDecl(NominalTypeDecl *NTD);

  void visitClassDecl(ClassDecl *CD);

  void visitConstructorDecl(ConstructorDecl *CD);

  void visitDestructorDecl(DestructorDecl *DD);

  void visitExtensionDecl(ExtensionDecl *ED);
  
  void visitFuncDecl(FuncDecl *FD);

  void visitProtocolDecl(ProtocolDecl *PD);

  void visitAbstractStorageDecl(AbstractStorageDecl *ASD);

  void visitVarDecl(VarDecl *VD);

  void visitEnumDecl(EnumDecl *ED);

  void visitEnumElementDecl(EnumElementDecl *EED);

  void visitDecl(Decl *D) {}

  void visit(Decl *D);
};
} // end namespace tbdgen
} // end namespace swift

#endif
