//===-- PrintAsObjC.cpp - Emit a header file for a Swift AST --------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "PrintAsObjC.h"
#include "swift/AST/AST.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

static void writeImports(raw_ostream &os, Module *M) {
  SmallVector<Module::ImportedModule, 16> imports;
  M->getImportedModules(imports, /*includePrivate=*/true);
  for (auto import : imports) {
    // FIXME: Handle submodule imports.
    os << "@import " << import.second->Name << ";\n";
  }
  os << "\n";
}

namespace {
class DeclWriter {
  enum class EmissionState {
    DefinitionRequested = 0,
    DefinitionInProgress,
    Defined
  };

  llvm::DenseMap<const TypeDecl *, std::pair<EmissionState, bool>> seenTypes;
  std::vector<const Decl *> declsToWrite;
  raw_ostream &os;
  Module &M;
public:
  DeclWriter(raw_ostream &out, Module &mod) : os(out), M(mod) {}

  bool isLocal(const Decl *D) {
    return D->getModuleContext() == &M;
  }

  bool require(const TypeDecl *D) {
    if (!isLocal(D))
      return true;

    auto &state = seenTypes[D];
    switch (state.first) {
    case EmissionState::DefinitionRequested:
      declsToWrite.push_back(D);
      return false;
    case EmissionState::DefinitionInProgress:
      llvm_unreachable("circular requirements");
    case EmissionState::Defined:
      return true;
    }
  }

  void forwardDeclare(const ClassDecl *CD) {
    if (!isLocal(CD))
      return;
    auto &state = seenTypes[CD];
    if (state.second)
      return;
    os << "@class " << CD->getName() << ";\n";
    state.second = true;
  }

  void forwardDeclare(const ProtocolDecl *PD) {
    if (!isLocal(PD))
      return;
    auto &state = seenTypes[PD];
    if (state.second)
      return;
    os << "@protocol " << PD->getName() << ";\n";
    state.second = true;
  }

  void printProtocols(ArrayRef<ProtocolDecl *> protos) {
    SmallVector<const ProtocolDecl *, 4> protosToPrint;
    std::copy_if(protos.begin(), protos.end(),
                 std::back_inserter(protosToPrint),
                 [](const ProtocolDecl *PD) -> bool {
      if (!PD->isObjC())
        return false;
      auto knownProtocol = PD->getKnownProtocolKind();
      if (!knownProtocol)
        return true;
      return *knownProtocol != KnownProtocolKind::DynamicLookup;
    });
    if (protosToPrint.empty())
      return;

    os << " <";
    interleave(protosToPrint,
               [this](const ProtocolDecl *PD) { os << PD->getName(); },
               [this] { os << ", "; });
    os << ">";
  }

  bool writeClass(const ClassDecl *CD) {
    if (!isLocal(CD))
      return true;

    auto &state = seenTypes[CD];
    if (state.first == EmissionState::Defined)
      return true;

    size_t pendingSize = declsToWrite.size();

    const ClassDecl *superclass = nullptr;
    if (Type superTy = CD->getSuperclass()) {
      superclass = superTy->getClassOrBoundGenericClass();
      require(superclass);
    }
    for (auto proto : CD->getProtocols())
      if (proto->isObjC())
        require(proto);

    if (declsToWrite.size() != pendingSize)
      return false;

    // FIXME: Include members.
    os << "@interface " << CD->getName();
    if (superclass)
      os << " : " << superclass->getName();
    printProtocols(CD->getProtocols());
    os << "\n@end\n";

    state = { EmissionState::Defined, true };
    return true;
  }

  bool writeProtocol(const ProtocolDecl *PD) {
    if (!isLocal(PD))
      return true;

    auto knownProtocol = PD->getKnownProtocolKind();
    if (knownProtocol && *knownProtocol == KnownProtocolKind::DynamicLookup)
      return true;

    auto &state = seenTypes[PD];
    if (state.first == EmissionState::Defined)
      return true;

    size_t pendingSize = declsToWrite.size();

    for (auto proto : PD->getProtocols()) {
      assert(proto->isObjC());
      require(proto);
    }

    if (declsToWrite.size() != pendingSize)
      return false;

    // FIXME: Include members.
    os << "@protocol " << PD->getName();
    printProtocols(PD->getProtocols());
    os << "\n@end\n";

    state = { EmissionState::Defined, true };
    return true;
  }

  bool writeExtension(const ExtensionDecl *ED, const ClassDecl *CD) {
    size_t pendingSize = declsToWrite.size();

    require(CD);
    for (auto proto : ED->getProtocols())
      if (proto->isObjC())
        require(proto);

    if (declsToWrite.size() != pendingSize)
      return false;

    // FIXME: Include members.
    os << "@interface " << CD->getName() << " ()";
    printProtocols(ED->getProtocols());
    os << "\n@end\n";
    return true;
  }

  bool writeDecls(ArrayRef<Decl *> decls = {}) {
    declsToWrite.reserve(declsToWrite.size() + decls.size());
    std::copy(decls.rbegin(), decls.rend(), std::back_inserter(declsToWrite));

    while (!declsToWrite.empty()) {
      const Decl *D = declsToWrite.back();
      bool success = true;

      if (auto VD = dyn_cast<ValueDecl>(D)) {
        // FIXME: Distinguish IBOutlet/IBAction from true interop.
        if (VD->isObjC()) {
          if (auto CD = dyn_cast<ClassDecl>(D))
            success = writeClass(CD);
          else if (auto PD = dyn_cast<ProtocolDecl>(D))
            success = writeProtocol(PD);
          else
            llvm_unreachable("unknown top-level ObjC value decl");
        }

      } else if (auto ED = dyn_cast<ExtensionDecl>(D)) {
        Type baseTy = ED->getExtendedType();
        auto theClass = baseTy->getClassOrBoundGenericClass();
        if (theClass && theClass->isObjC())
          success = writeExtension(ED, theClass);
      }

      if (success) {
        assert(declsToWrite.back() == D);
        declsToWrite.pop_back();
      }
    }

    return false;
  }
};
}

static bool writeDecls(raw_ostream &os, Module *M) {
  SmallVector<Decl *, 64> decls;
  M->getTopLevelDecls(decls);
  return DeclWriter(os, *M).writeDecls(decls);
}

int swift::doPrintAsObjC(const CompilerInvocation &InitInvok) {
  CompilerInstance CI;
  PrintingDiagnosticConsumer PrintDiags;
  CI.addDiagnosticConsumer(&PrintDiags);

  if (CI.setup(InitInvok))
    return 1;
  CI.performParse();

  if (CI.getASTContext().hadError())
    return 1;

  writeImports(llvm::outs(), CI.getMainModule());
  bool HadError = writeDecls(llvm::outs(), CI.getMainModule());

  return HadError;
}
