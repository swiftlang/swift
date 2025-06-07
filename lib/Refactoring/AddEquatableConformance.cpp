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
#include "swift/AST/ParameterList.h"
#include "swift/AST/TypeRepr.h"

using namespace swift::refactoring;

namespace {
class AddEquatableContext {

  /// Declaration context
  DeclContext *DC;

  /// Adopter type
  Type Adopter;

  /// Start location of declaration context brace
  SourceLoc StartLoc;

  /// Array of all inherited protocols' locations
  ArrayRef<InheritedEntry> ProtocolsLocations;

  /// Array of all conformed protocols
  SmallVector<swift::ProtocolDecl *, 2> Protocols;

  /// Start location of declaration,
  /// a place to write protocol name
  SourceLoc ProtInsertStartLoc;

  /// Stored properties of extending adopter
  ArrayRef<VarDecl *> StoredProperties;

  /// Range of internal members in declaration
  DeclRange Range;

  bool conformsToEquatableProtocol() {
    for (ProtocolDecl *Protocol : Protocols) {
      if (Protocol->getKnownProtocolKind() == KnownProtocolKind::Equatable) {
        return true;
      }
    }
    return false;
  }

  bool isRequirementValid() {
    auto Reqs = getProtocolRequirements();
    if (Reqs.empty()) {
      return false;
    }
    auto Req = dyn_cast<FuncDecl>(Reqs[0]);
    return Req && Req->getParameters()->size() == 2;
  }

  bool isPropertiesListValid() {
    return !getUserAccessibleProperties().empty();
  }

  void printFunctionBody(ASTPrinter &Printer, StringRef ExtraIndent,
                         ParameterList *Params);

  std::vector<ValueDecl *> getProtocolRequirements();

  std::vector<VarDecl *> getUserAccessibleProperties();

public:
  AddEquatableContext(NominalTypeDecl *Decl)
      : DC(Decl), Adopter(Decl->getDeclaredType()),
        StartLoc(Decl->getBraces().Start),
        ProtocolsLocations(Decl->getInherited().getEntries()),
        Protocols(getAllProtocols(Decl)),
        ProtInsertStartLoc(Decl->getNameLoc()),
        StoredProperties(Decl->getStoredProperties()),
        Range(Decl->getMembers()){};

  AddEquatableContext(ExtensionDecl *Decl)
      : DC(Decl), Adopter(Decl->getExtendedType()),
        StartLoc(Decl->getBraces().Start),
        ProtocolsLocations(Decl->getInherited().getEntries()),
        Protocols(getAllProtocols(Decl->getExtendedNominal())),
        ProtInsertStartLoc(Decl->getExtendedTypeRepr()->getEndLoc()),
        StoredProperties(Decl->getExtendedNominal()->getStoredProperties()),
        Range(Decl->getMembers()){};

  AddEquatableContext()
      : DC(nullptr), Adopter(), ProtocolsLocations(), Protocols(),
        StoredProperties(), Range(nullptr, nullptr){};

  static AddEquatableContext
  getDeclarationContextFromInfo(ResolvedCursorInfoPtr Info);

  std::string getInsertionTextForProtocol();

  std::string getInsertionTextForFunction(SourceManager &SM);

  bool isValid() {
    // FIXME: Allow to generate explicit == method for declarations which
    // already have compiler-generated == method
    return StartLoc.isValid() && ProtInsertStartLoc.isValid() &&
           !conformsToEquatableProtocol() && isPropertiesListValid() &&
           isRequirementValid();
  }

  SourceLoc getStartLocForProtocolDecl() {
    if (ProtocolsLocations.empty()) {
      return ProtInsertStartLoc;
    }
    return ProtocolsLocations.back().getSourceRange().Start;
  }

  bool isMembersRangeEmpty() { return Range.empty(); }

  SourceLoc getInsertStartLoc();
};

SourceLoc AddEquatableContext::getInsertStartLoc() {
  SourceLoc MaxLoc = StartLoc;
  for (auto Mem : Range) {
    if (Mem->getEndLoc().getOpaquePointerValue() >
        MaxLoc.getOpaquePointerValue()) {
      MaxLoc = Mem->getEndLoc();
    }
  }
  return MaxLoc;
}

std::string AddEquatableContext::getInsertionTextForProtocol() {
  StringRef ProtocolName = getProtocolName(KnownProtocolKind::Equatable);
  std::string Buffer;
  llvm::raw_string_ostream OS(Buffer);
  if (ProtocolsLocations.empty()) {
    OS << ": " << ProtocolName;
    return Buffer;
  }
  OS << ", " << ProtocolName;
  return Buffer;
}

std::string
AddEquatableContext::getInsertionTextForFunction(SourceManager &SM) {
  auto Reqs = getProtocolRequirements();
  auto Req = dyn_cast<FuncDecl>(Reqs[0]);
  auto Params = Req->getParameters();
  StringRef ExtraIndent;
  StringRef CurrentIndent =
      Lexer::getIndentationForLine(SM, getInsertStartLoc(), &ExtraIndent);
  std::string Indent;
  if (isMembersRangeEmpty()) {
    Indent = (CurrentIndent + ExtraIndent).str();
  } else {
    Indent = CurrentIndent.str();
  }
  PrintOptions Options = PrintOptions::printVerbose();
  Options.PrintDocumentationComments = false;
  Options.setBaseType(Adopter);
  Options.FunctionBody = [&](const ValueDecl *VD, ASTPrinter &Printer) {
    Printer << " {";
    Printer.printNewline();
    printFunctionBody(Printer, ExtraIndent, Params);
    Printer.printNewline();
    Printer << "}";
  };
  std::string Buffer;
  llvm::raw_string_ostream OS(Buffer);
  ExtraIndentStreamPrinter Printer(OS, Indent);
  Printer.printNewline();
  if (!isMembersRangeEmpty()) {
    Printer.printNewline();
  }
  Reqs[0]->print(Printer, Options);
  return Buffer;
}

std::vector<VarDecl *> AddEquatableContext::getUserAccessibleProperties() {
  std::vector<VarDecl *> PublicProperties;
  for (VarDecl *Decl : StoredProperties) {
    if (Decl->Decl::isUserAccessible()) {
      PublicProperties.push_back(Decl);
    }
  }
  return PublicProperties;
}

std::vector<ValueDecl *> AddEquatableContext::getProtocolRequirements() {
  std::vector<ValueDecl *> Collection;
  auto Proto = DC->getASTContext().getProtocol(KnownProtocolKind::Equatable);
  for (auto Member : Proto->getMembers()) {
    auto Req = dyn_cast<ValueDecl>(Member);
    if (!Req || Req->isInvalid() || !Req->isProtocolRequirement()) {
      continue;
    }
    Collection.push_back(Req);
  }
  return Collection;
}

AddEquatableContext
AddEquatableContext::getDeclarationContextFromInfo(ResolvedCursorInfoPtr Info) {
  auto ValueRefInfo = dyn_cast<ResolvedValueRefCursorInfo>(Info);
  if (!ValueRefInfo) {
    return AddEquatableContext();
  }
  if (!ValueRefInfo->isRef()) {
    if (auto *NomDecl = dyn_cast<NominalTypeDecl>(ValueRefInfo->getValueD())) {
      return AddEquatableContext(NomDecl);
    }
  } else if (auto *ExtDecl = ValueRefInfo->getExtTyRef()) {
    if (ExtDecl->getExtendedNominal()) {
      return AddEquatableContext(ExtDecl);
    }
  }
  return AddEquatableContext();
}

void AddEquatableContext::printFunctionBody(ASTPrinter &Printer,
                                            StringRef ExtraIndent,
                                            ParameterList *Params) {
  SmallString<128> Return;
  llvm::raw_svector_ostream SS(Return);
  SS << tok::kw_return;
  StringRef Space = " ";
  StringRef AdditionalSpace = "       ";
  StringRef Point = ".";
  StringRef Join = " == ";
  StringRef And = " &&";
  auto Props = getUserAccessibleProperties();
  auto FParam = Params->get(0)->getName();
  auto SParam = Params->get(1)->getName();
  auto Prop = Props[0]->getName();
  Printer << ExtraIndent << Return << Space << FParam << Point << Prop << Join
          << SParam << Point << Prop;
  if (Props.size() > 1) {
    std::for_each(Props.begin() + 1, Props.end(), [&](VarDecl *VD) {
      auto Name = VD->getName();
      Printer << And;
      Printer.printNewline();
      Printer << ExtraIndent << AdditionalSpace << FParam << Point << Name
              << Join << SParam << Point << Name;
    });
  }
}
} // namespace

bool RefactoringActionAddEquatableConformance::isApplicable(
    ResolvedCursorInfoPtr Tok, DiagnosticEngine &Diag) {
  return AddEquatableContext::getDeclarationContextFromInfo(Tok).isValid();
}

bool RefactoringActionAddEquatableConformance::performChange() {
  auto Context = AddEquatableContext::getDeclarationContextFromInfo(CursorInfo);
  EditConsumer.insertAfter(SM, Context.getStartLocForProtocolDecl(),
                           Context.getInsertionTextForProtocol());
  EditConsumer.insertAfter(SM, Context.getInsertStartLoc(),
                           Context.getInsertionTextForFunction(SM));
  return false;
}
