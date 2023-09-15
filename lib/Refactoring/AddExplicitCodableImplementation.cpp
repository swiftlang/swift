//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "RefactoringActions.h"
#include "Utils.h"

using namespace swift::refactoring;

namespace {
class AddCodableContext {

  /// Declaration context
  DeclContext *DC;

  /// Start location of declaration context brace
  SourceLoc StartLoc;

  /// Array of all conformed protocols
  SmallVector<swift::ProtocolDecl *, 2> Protocols;

  /// Range of internal members in declaration
  DeclRange Range;

  bool conformsToCodableProtocol() {
    for (ProtocolDecl *Protocol : Protocols) {
      if (Protocol->getKnownProtocolKind() == KnownProtocolKind::Encodable ||
          Protocol->getKnownProtocolKind() == KnownProtocolKind::Decodable) {
        return true;
      }
    }
    return false;
  }

public:
  AddCodableContext(NominalTypeDecl *Decl)
      : DC(Decl), StartLoc(Decl->getBraces().Start),
        Protocols(getAllProtocols(Decl)), Range(Decl->getMembers()){};

  AddCodableContext(ExtensionDecl *Decl)
      : DC(Decl), StartLoc(Decl->getBraces().Start),
        Protocols(getAllProtocols(Decl->getExtendedNominal())),
        Range(Decl->getMembers()){};

  AddCodableContext() : DC(nullptr), Protocols(), Range(nullptr, nullptr){};

  static AddCodableContext
  getDeclarationContextFromInfo(ResolvedCursorInfoPtr Info);

  void printInsertionText(ResolvedCursorInfoPtr CursorInfo, SourceManager &SM,
                          llvm::raw_ostream &OS);

  bool isValid() { return StartLoc.isValid() && conformsToCodableProtocol(); }

  SourceLoc getInsertStartLoc();
};

SourceLoc AddCodableContext::getInsertStartLoc() {
  SourceLoc MaxLoc = StartLoc;
  for (auto Mem : Range) {
    if (Mem->getEndLoc().getOpaquePointerValue() >
        MaxLoc.getOpaquePointerValue()) {
      MaxLoc = Mem->getEndLoc();
    }
  }
  return MaxLoc;
}

/// Walks an AST and prints the synthesized Codable implementation.
class SynthesizedCodablePrinter : public ASTWalker {
private:
  ASTPrinter &Printer;

public:
  SynthesizedCodablePrinter(ASTPrinter &Printer) : Printer(Printer) {}

  MacroWalking getMacroWalkingBehavior() const override {
    return MacroWalking::Arguments;
  }

  PreWalkAction walkToDeclPre(Decl *D) override {
    auto *VD = dyn_cast<ValueDecl>(D);
    if (!VD)
      return Action::SkipChildren();

    if (!VD->isSynthesized()) {
      return Action::Continue();
    }
    SmallString<32> Scratch;
    auto name = VD->getName().getString(Scratch);
    // Print all synthesized enums,
    // since Codable can synthesize multiple enums (for associated values).
    auto shouldPrint =
        isa<EnumDecl>(VD) || name == "init(from:)" || name == "encode(to:)";
    if (!shouldPrint) {
      // Some other synthesized decl that we don't want to print.
      return Action::SkipChildren();
    }

    Printer.printNewline();

    if (auto enumDecl = dyn_cast<EnumDecl>(D)) {
      // Manually print enum here, since we don't want to print synthesized
      // functions.
      Printer << "enum " << enumDecl->getNameStr();
      PrintOptions Options;
      Options.PrintSpaceBeforeInheritance = false;
      enumDecl->printInherited(Printer, Options);
      Printer << " {";
      for (Decl *EC : enumDecl->getAllElements()) {
        Printer.printNewline();
        Printer << "  ";
        EC->print(Printer, Options);
      }
      Printer.printNewline();
      Printer << "}";
      return Action::SkipChildren();
    }

    PrintOptions Options;
    Options.SynthesizeSugarOnTypes = true;
    Options.FunctionDefinitions = true;
    Options.VarInitializers = true;
    Options.PrintExprs = true;
    Options.TypeDefinitions = true;
    Options.ExcludeAttrList.push_back(DAK_HasInitialValue);

    Printer.printNewline();
    D->print(Printer, Options);

    return Action::SkipChildren();
  }
};

void AddCodableContext::printInsertionText(ResolvedCursorInfoPtr CursorInfo,
                                           SourceManager &SM,
                                           llvm::raw_ostream &OS) {
  StringRef ExtraIndent;
  StringRef CurrentIndent =
      Lexer::getIndentationForLine(SM, getInsertStartLoc(), &ExtraIndent);
  std::string Indent;
  if (getInsertStartLoc() == StartLoc) {
    Indent = (CurrentIndent + ExtraIndent).str();
  } else {
    Indent = CurrentIndent.str();
  }

  ExtraIndentStreamPrinter Printer(OS, Indent);
  Printer.printNewline();
  SynthesizedCodablePrinter Walker(Printer);
  DC->getAsDecl()->walk(Walker);
}

AddCodableContext
AddCodableContext::getDeclarationContextFromInfo(ResolvedCursorInfoPtr Info) {
  auto ValueRefInfo = dyn_cast<ResolvedValueRefCursorInfo>(Info);
  if (!ValueRefInfo) {
    return AddCodableContext();
  }
  if (!ValueRefInfo->isRef()) {
    if (auto *NomDecl = dyn_cast<NominalTypeDecl>(ValueRefInfo->getValueD())) {
      return AddCodableContext(NomDecl);
    }
  }
  // TODO: support extensions
  // (would need to get synthesized nodes from the main decl,
  // and only if it's in the same file?)
  return AddCodableContext();
}
} // namespace

bool RefactoringActionAddExplicitCodableImplementation::isApplicable(
    ResolvedCursorInfoPtr Tok, DiagnosticEngine &Diag) {
  return AddCodableContext::getDeclarationContextFromInfo(Tok).isValid();
}

bool RefactoringActionAddExplicitCodableImplementation::performChange() {
  auto Context = AddCodableContext::getDeclarationContextFromInfo(CursorInfo);

  SmallString<64> Buffer;
  llvm::raw_svector_ostream OS(Buffer);
  Context.printInsertionText(CursorInfo, SM, OS);

  EditConsumer.insertAfter(SM, Context.getInsertStartLoc(), OS.str());
  return false;
}
