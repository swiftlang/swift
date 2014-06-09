//===--- ASTPrinter.cpp - Swift Language AST Printer---------------------===//
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
//
//  This file implements printing for the Swift ASTs.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ArchetypeBuilder.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/PrintOptions.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/TypeVisitor.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Basic/PrimitiveParsing.h"
#include "swift/Basic/STLExtras.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SaveAndRestore.h"
#include <algorithm>

using namespace swift;

void ASTPrinter::anchor() {}

void ASTPrinter::printIndent() {
  llvm::SmallString<16> Str;
  for (unsigned i = 0; i != CurrentIndentation; ++i)
    Str += ' ';
  
  printText(Str);
}

void ASTPrinter::printTextImpl(StringRef Text) {
  if (PendingNewlines != 0) {
    llvm::SmallString<16> Str;
    for (unsigned i = 0; i != PendingNewlines; ++i)
      Str += '\n';
    PendingNewlines = 0;

    printText(Str);
    printIndent();
  }
  
  const Decl *PreD = PendingDeclPreCallback;
  const Decl *LocD = PendingDeclLocCallback;
  PendingDeclPreCallback = nullptr;
  PendingDeclLocCallback = nullptr;
  
  if (PreD) {
    printDeclPre(PreD);
  }
  if (LocD) {
    printDeclLoc(LocD);
  }

  printText(Text);
}

ASTPrinter &ASTPrinter::operator<<(unsigned long long N) {
  llvm::SmallString<32> Str;
  llvm::raw_svector_ostream OS(Str);
  OS << N;
  printTextImpl(OS.str());
  return *this;
}

void ASTPrinter::printName(Identifier Name) {
  if (Name.empty()) {
    *this << "_";
    return;
  }
  bool IsKeyword = llvm::StringSwitch<bool>(Name.str())
#define KEYWORD(KW) \
      .Case(#KW, true)
#include "swift/Parse/Tokens.def"
      .Default(false);
  if (IsKeyword)
    *this << "`";
  *this << Name.str();
  if (IsKeyword)
    *this << "`";
}

void StreamPrinter::printText(StringRef Text) {
  OS << Text;
}

namespace {
/// \brief AST pretty-printer.
class PrintAST : public ASTVisitor<PrintAST> {
  ASTPrinter &Printer;
  PrintOptions Options;
  unsigned IndentLevel = 0;

  friend DeclVisitor<PrintAST>;

  /// \brief RAII object that increases the indentation level.
  class IndentRAII {
    PrintAST &Self;
    bool DoIndent;

  public:
    IndentRAII(PrintAST &self, bool DoIndent = true)
        : Self(self), DoIndent(DoIndent) {
      if (DoIndent)
        Self.IndentLevel += Self.Options.Indent;
    }

    ~IndentRAII() {
      if (DoIndent)
        Self.IndentLevel -= Self.Options.Indent;
    }
  };

  /// \brief Indent the current number of indentation spaces.
  void indent() {
    Printer.setIndent(IndentLevel);
  }

  /// \brief Record the location of this declaration, which is about to
  /// be printed.
  void recordDeclLoc(Decl *decl) {
    Printer.callPrintDeclLoc(decl);
  }

  void printClangDocumentationComment(const clang::Decl *D) {
    const auto &ClangContext = D->getASTContext();
    const clang::RawComment *RC = ClangContext.getRawCommentForAnyRedecl(D);
    if (!RC)
      return;

    if (!Options.PrintRegularClangComments) {
      Printer.printNewline();
      indent();
    }

    bool Invalid;
    unsigned StartLocCol =
        ClangContext.getSourceManager().getSpellingColumnNumber(
            RC->getLocStart(), &Invalid);
    if (Invalid)
      StartLocCol = 0;

    unsigned WhitespaceToTrim = StartLocCol ? StartLocCol - 1 : 0;

    SmallVector<StringRef, 8> Lines;

    StringRef RawText =
        RC->getRawText(ClangContext.getSourceManager()).rtrim("\n\r");
    trimLeadingWhitespaceFromLines(RawText, WhitespaceToTrim, Lines);

    for (auto Line : Lines) {
      Printer << Line;
      Printer.printNewline();
    }
  }

  void printSwiftDocumentationComment(const Decl *D) {
    auto RC = D->getRawComment();
    if (RC.isEmpty())
      return;

    Printer.printNewline();
    indent();

    SmallVector<StringRef, 8> Lines;
    for (const auto &SRC : RC.Comments) {
      Lines.clear();

      StringRef RawText = SRC.RawText.rtrim("\n\r");
      unsigned WhitespaceToTrim = SRC.StartColumn - 1;
      trimLeadingWhitespaceFromLines(RawText, WhitespaceToTrim, Lines);

      for (auto Line : Lines) {
        Printer << Line;
        Printer.printNewline();
      }
    }
  }

  void printDocumentationComment(Decl *D) {
    if (!Options.PrintDocumentationComments)
      return;

    // Try to print a comment from Clang.
    auto MaybeClangNode = D->getClangNode();
    if (MaybeClangNode) {
      if (auto *CD = MaybeClangNode.getAsDecl())
        printClangDocumentationComment(CD);
      return;
    }

    printSwiftDocumentationComment(D);
  }

  void printStaticKeyword(StaticSpellingKind StaticSpelling) {
    switch (StaticSpelling) {
    case StaticSpellingKind::None:
      llvm_unreachable("should not be called for non-static decls");
    case StaticSpellingKind::KeywordStatic:
      Printer << "static ";
      break;
    case StaticSpellingKind::KeywordClass:
      Printer<< "class ";
      break;
    }
  }

  void printOverrideKeyword(Decl *D) {
    if (Options.PrintOverrideKeyword &&
        D->getAttrs().hasAttribute<OverrideAttr>())
      Printer << "override ";
  }

  void printTypeLoc(const TypeLoc &TL) {
    // Print a TypeRepr if instructed to do so by options, or if the type
    // is null.
    if ((Options.PreferTypeRepr && TL.hasLocation()) ||
        TL.getType().isNull()) {
      TL.getTypeRepr()->print(Printer, Options);
      return;
    }
    TL.getType().print(Printer, Options);
  }

  void printAttributes(const Decl *D);
  void printTypedPattern(const TypedPattern *TP,
                         bool StripOuterSliceType = false);

public:
  void printPattern(const Pattern *pattern);

  void printGenericParams(GenericParamList *params);

private:
  bool shouldPrint(const Decl *D);
  void printAccessors(AbstractStorageDecl *ASD);
  void printMembers(DeclRange members, bool needComma = false);
  void printNominalDeclName(NominalTypeDecl *decl);
  void printInherited(const Decl *decl,
                      ArrayRef<TypeLoc> inherited,
                      ArrayRef<ProtocolDecl *> protos,
                      Type superclass = {},
                      bool PrintAsProtocolComposition = false);

  template <typename DeclWithSuperclass>
  void printInheritedWithSuperclass(DeclWithSuperclass *decl);

  void printInherited(const TypeDecl *decl);
  void printInherited(const EnumDecl *D);
  void printInherited(const ExtensionDecl *decl);
  void printInherited(const GenericTypeParamDecl *D);

  void printEnumElement(EnumElementDecl *elt);

  /// \returns true if anything was printed.
  bool printBraceStmtElements(BraceStmt *stmt, bool NeedIndent = true);

  void printOneParameter(Identifier ArgName,
                         const Pattern *BodyPattern,
                         bool ArgNameIsAPIByDefault,
                         bool StripOuterSliceType,
                         bool Curried);

  /// \brief Print the function parameters in curried or selector style,
  /// to match the original function declaration.
  void printFunctionParameters(AbstractFunctionDecl *AFD);

#define DECL(Name,Parent) void visit##Name##Decl(Name##Decl *decl);
#define ABSTRACT_DECL(Name, Parent)
#define DECL_RANGE(Name,Start,End)
#include "swift/AST/DeclNodes.def"

#define STMT(Name, Parent) void visit##Name##Stmt(Name##Stmt *stmt);
#include "swift/AST/StmtNodes.def"

public:
  PrintAST(ASTPrinter &Printer, const PrintOptions &Options)
      : Printer(Printer), Options(Options) {}

  using ASTVisitor::visit;

  bool visit(Decl *D) {
    if (!shouldPrint(D))
      return false;

    Printer.callPrintDeclPre(D);
    ASTVisitor::visit(D);
    Printer.printDeclPost(D);
    return true;
  }
};
} // unnamed namespace

void PrintAST::printAttributes(const Decl *D) {
  if (Options.SkipAttributes)
    return;
  D->getAttrs().print(Printer, Options);
}

void PrintAST::printTypedPattern(const TypedPattern *TP,
                                 bool StripOuterSliceType) {
  auto TheTypeLoc = TP->getTypeLoc();
  if (TheTypeLoc.hasLocation()) {
    // If the outer typeloc is an InOutTypeRepr, print the inout before the
    // subpattern.
    if (auto *IOT = dyn_cast<InOutTypeRepr>(TheTypeLoc.getTypeRepr())) {
      TheTypeLoc = TypeLoc(IOT->getBase());
      Type T = TheTypeLoc.getType();
      if (T) {
        if (auto *IOT = T->getAs<InOutType>()) {
          T = IOT->getObjectType();
          TheTypeLoc.setType(T);
        }
      }

      Printer << "inout ";
    }

    printPattern(TP->getSubPattern());
    Printer << ": ";
    if (StripOuterSliceType) {
      Type T = TP->getType();
      if (auto *BGT = T->getAs<BoundGenericType>()) {
        BGT->getGenericArgs()[0].print(Printer, Options);
        return;
      }
    }
    printTypeLoc(TheTypeLoc);
    return;
  }

  Type T = TP->getType();
  if (auto *IOT = T->getAs<InOutType>()) {
    T = IOT->getObjectType();
    Printer << "inout ";
  }

  printPattern(TP->getSubPattern());
  Printer << ": ";
  if (StripOuterSliceType) {
    if (auto *BGT = T->getAs<BoundGenericType>()) {
      BGT->getGenericArgs()[0].print(Printer, Options);
      return;
    }
  }
  T.print(Printer, Options);
}

void PrintAST::printPattern(const Pattern *pattern) {
  switch (pattern->getKind()) {
  case PatternKind::Any:
    Printer << "_";
    break;

  case PatternKind::Named: {
    auto named = cast<NamedPattern>(pattern);
    recordDeclLoc(named->getDecl());
    Printer.printName(named->getBoundName());
    break;
  }

  case PatternKind::Paren:
    Printer << "(";
    printPattern(cast<ParenPattern>(pattern)->getSubPattern());
    Printer << ")";
    break;

  case PatternKind::Tuple: {
    Printer << "(";
    auto TP = cast<TuplePattern>(pattern);
    auto Fields = TP->getFields();
    for (unsigned i = 0, e = Fields.size(); i != e; ++i) {
      const auto &Elt = Fields[i];
      if (i != 0)
        Printer << ", ";

      if (i == e - 1 && TP->hasVararg()) {
        printTypedPattern(cast<TypedPattern>(Elt.getPattern()),
                          /*StripOuterSliceType=*/true);
      } else {
        printPattern(Elt.getPattern());
      }
      if (Elt.getDefaultArgKind() != DefaultArgumentKind::None) {
        if (Options.PrintDefaultParameterPlaceholder)
          Printer << " = default";
        else if (Options.VarInitializers) {
          // FIXME: Print initializer here.
        }
      }
    }
    if (TP->hasVararg())
      Printer << "...";
    Printer << ")";
    break;
  }

  case PatternKind::Typed:
    printTypedPattern(cast<TypedPattern>(pattern));
    break;

  case PatternKind::Isa: {
    auto isa = cast<IsaPattern>(pattern);
    Printer << "is ";
    isa->getCastTypeLoc().getType().print(Printer, Options);
    break;
  }

  case PatternKind::NominalType: {
    auto type = cast<NominalTypePattern>(pattern);
    type->getCastTypeLoc().getType().print(Printer, Options);
    Printer << "(";
    interleave(type->getElements().begin(), type->getElements().end(),
               [&](const NominalTypePattern::Element &elt) {
                 Printer << elt.getPropertyName().str() << ":";
                 printPattern(elt.getSubPattern());
               }, [&] {
                 Printer << ", ";
               });
    break;
  }

  case PatternKind::EnumElement: {
    auto elt = cast<EnumElementPattern>(pattern);
    // FIXME: Print element expr.
    if (elt->hasSubPattern())
      printPattern(elt->getSubPattern());
    break;
  }

  case PatternKind::Expr:
    // FIXME: Print expr.
    break;

  case PatternKind::Var:
    if (!Options.SkipIntroducerKeywords)
      Printer << "var ";
    printPattern(cast<VarPattern>(pattern)->getSubPattern());
  }
}

void PrintAST::printGenericParams(GenericParamList *Params) {
  if (!Params)
    return;

  Printer << "<";
  bool IsFirst = true;
  for (auto GP : Params->getParams()) {
    if (IsFirst) {
      IsFirst = false;
    } else {
      Printer << ", ";
    }

    auto TypeParam = GP.getAsTypeParam();
    Printer.printName(TypeParam->getName());
    printInherited(TypeParam);
  }

  auto Requirements = Params->getRequirements();
  if (!Requirements.empty()) {
    bool IsFirst = true;
    for (auto &Req : Requirements) {
      if (Req.isInvalid() ||
          Req.getKind() == RequirementKind::WitnessMarker)
        continue;

      if (IsFirst) {
        Printer << " where ";
        IsFirst = false;
      } else {
        Printer << ", ";
      }

      switch (Req.getKind()) {
      case RequirementKind::Conformance:
        printTypeLoc(Req.getSubjectLoc());
        Printer << " : ";
        printTypeLoc(Req.getConstraintLoc());
        break;
      case RequirementKind::SameType:
        printTypeLoc(Req.getFirstTypeLoc());
        Printer << " == ";
        printTypeLoc(Req.getSecondTypeLoc());
        break;
      case RequirementKind::WitnessMarker:
        llvm_unreachable("Handled above");
      }
    }
  }
  Printer << ">";
}

bool PrintAST::shouldPrint(const Decl *D) {
  if (Options.SkipImplicit && D->isImplicit())
    return false;

  if (Options.SkipUnavailable && D->getAttrs().isUnavailable())
    return false;

  if (Options.SkipPrivateStdlibDecls && D->isPrivateStdlibDecl())
      return false;

  if (auto Ext = dyn_cast<ExtensionDecl>(D)) {
    // If the extension doesn't add protocols or has no members that we should
    // print then skip printing it.
    if (Ext->getProtocols().empty()) {
      bool HasMemberToPrint = false;
      for (auto Member : Ext->getMembers()) {
        if (shouldPrint(Member)) {
          HasMemberToPrint = true;
          break;
        }
      }
      if (!HasMemberToPrint)
        return false;
    }
  }
  return true;
}

void PrintAST::printAccessors(AbstractStorageDecl *ASD) {
  if (!ASD->hasAccessorFunctions() ||
      ASD->getStorageKind() == AbstractStorageDecl::StoredWithTrivialAccessors){
    // This is a 'let' vardecl.  We could print the initializer if we could
    // print expressions.
    return;
  }

  if (Options.PrintGetSetOnRWProperties && !Options.FunctionDefinitions &&
      (ASD->getGetter() || ASD->getSetter())) {
    Printer << " {";
    if (ASD->getGetter()) Printer << " get";
    if (ASD->getSetter()) Printer << " set";
    Printer << " }";
    return;
  }

  bool InProtocol = isa<ProtocolDecl>(ASD->getDeclContext());
  if (!InProtocol && !Options.FunctionDefinitions &&
      !Options.PrintGetSetOnRWProperties &&
      ASD->getGetter() && ASD->getSetter())
    return;

  bool PrintAccessorBody = Options.FunctionDefinitions && !InProtocol;

  auto PrintAccessor = [&](AbstractFunctionDecl *Accessor, StringRef Label) {
    if (!Accessor)
      return;
    if (!PrintAccessorBody)
      Printer << " " << Label;
    else {
      Printer.printNewline();
      IndentRAII IndentMore(*this);
      indent();
      visit(Accessor);
    }
  };

  Printer << " {";
  if (ASD->getStorageKind() == VarDecl::Observing) {
    PrintAccessor(ASD->getWillSetFunc(), "willSet");
    PrintAccessor(ASD->getDidSetFunc(), "didSet");
  } else {
    PrintAccessor(ASD->getGetter(), "get");
    PrintAccessor(ASD->getSetter(), "set");
  }
  if (PrintAccessorBody) {
    Printer.printNewline();
    indent();
  } else
    Printer << " ";
  Printer << "}";
}

void PrintAST::printMembers(DeclRange members, bool needComma) {
  Printer << " {";
  Printer.printNewline();
  {
    IndentRAII indentMore(*this);
    for (auto i = members.begin(), iEnd = members.end(); i != iEnd; ++i) {
      auto member = *i;

      if (!shouldPrint(member))
        continue;

      if (!member->shouldPrintInContext(Options))
        continue;

      indent();
      visit(member);
      if (needComma && std::next(i) != iEnd)
        Printer << ",";
      Printer.printNewline();
    }
  }
  indent();
  Printer << "}";
}

void PrintAST::printNominalDeclName(NominalTypeDecl *decl) {
  Printer.printName(decl->getName());
  if (auto gp = decl->getGenericParams()) {
    if (!isa<ProtocolDecl>(decl))
      printGenericParams(gp);
  }
}

void PrintAST::printInherited(const Decl *decl,
                              ArrayRef<TypeLoc> inherited,
                              ArrayRef<ProtocolDecl *> protos,
                              Type superclass,
                              bool PrintAsProtocolComposition) {
  if (inherited.empty() && superclass.isNull()) {
    if (protos.empty())
      return;
    // If only conforms to AnyObject protocol, nothing to print.
    if (protos.size() == 1) {
      if (protos.front()->isSpecificProtocol(KnownProtocolKind::AnyObject))
        return;
    }
  }

  if (inherited.empty()) {
    bool PrintedColon = false;
    bool PrintedInherited = false;

    if (superclass) {
      bool ShouldPrintSuper = true;
      if (auto NTD = superclass->getAnyNominal()) {
        ShouldPrintSuper = shouldPrint(NTD);
      }
      if (ShouldPrintSuper) {
        Printer << " : ";
        superclass.print(Printer, Options);
        PrintedInherited = true;
      }
    }

    bool UseProtocolCompositionSyntax =
        PrintAsProtocolComposition && protos.size() > 1;
    if (UseProtocolCompositionSyntax) {
      Printer << " : protocol<";
      PrintedColon = true;
    }
    for (auto Proto : protos) {
      if (!shouldPrint(Proto))
        continue;
      if (Proto->isSpecificProtocol(KnownProtocolKind::AnyObject))
        continue;
      if (auto Enum = dyn_cast<EnumDecl>(decl)) {
        // Conformance to RawRepresentable is implied by having a raw type.
        if (Enum->hasRawType()
            && Proto->isSpecificProtocol(KnownProtocolKind::RawRepresentable))
          continue;
        // Conformance to Equatable and Hashable is implied by being a "simple"
        // no-payload enum.
        if (Enum->isSimpleEnum()
            && (Proto->isSpecificProtocol(KnownProtocolKind::Equatable)
                || Proto->isSpecificProtocol(KnownProtocolKind::Hashable)))
          continue;
      }

      if (PrintedInherited)
        Printer << ", ";
      else if (!PrintedColon)
        Printer << " : ";
      Proto->getDeclaredType()->print(Printer, Options);
      PrintedInherited = true;
      PrintedColon = true;
    }
    if (UseProtocolCompositionSyntax)
      Printer << ">";
  } else {
    SmallVector<Type, 6> TypesToPrint;
    for (auto TL : inherited) {
      if (auto NTD = TL.getType()->getAnyNominal()) {
        if (!shouldPrint(NTD))
          continue;
      }
      TypesToPrint.push_back(TL.getType());
    }
    if (TypesToPrint.empty())
      return;

    Printer << " : ";

    interleave(TypesToPrint, [&](Type Ty) {
      Ty->print(Printer, Options);
    }, [&]() {
      Printer << ", ";
    });
  }
}

template <typename DeclWithSuperclass>
void PrintAST::printInheritedWithSuperclass(DeclWithSuperclass *decl) {
  printInherited(decl, decl->getInherited(), decl->getProtocols(),
                 decl->getSuperclass());
}

void PrintAST::printInherited(const TypeDecl *decl) {
  printInherited(decl, decl->getInherited(), decl->getProtocols());
}

void PrintAST::printInherited(const EnumDecl *D) {
  printInherited(D, D->getInherited(), D->getProtocols(), D->getRawType());
}

void PrintAST::printInherited(const ExtensionDecl *decl) {
  printInherited(decl, decl->getInherited(), decl->getProtocols());
}

void PrintAST::printInherited(const GenericTypeParamDecl *D) {
  printInherited(D, D->getInherited(), D->getProtocols(), D->getSuperclass(),
                 true);
}

void PrintAST::visitImportDecl(ImportDecl *decl) {
  printAttributes(decl);
  Printer << "import ";

  switch (decl->getImportKind()) {
  case ImportKind::Module:
    break;
  case ImportKind::Type:
    Printer << "typealias ";
    break;
  case ImportKind::Struct:
    Printer << "struct ";
    break;
  case ImportKind::Class:
    Printer << "class ";
    break;
  case ImportKind::Enum:
    Printer << "enum ";
    break;
  case ImportKind::Protocol:
    Printer << "protocol ";
    break;
  case ImportKind::Var:
    Printer << "var ";
    break;
  case ImportKind::Func:
    Printer << "func ";
    break;
  }
  recordDeclLoc(decl);
  interleave(decl->getFullAccessPath(),
             [&](const ImportDecl::AccessPathElement &Elem) {
               Printer << Elem.first.str();
             },
             [&] { Printer << "."; });
}

void PrintAST::visitExtensionDecl(ExtensionDecl *decl) {
  Printer << "extension ";
  recordDeclLoc(decl);
  decl->getExtendedType().print(Printer, Options);
  printInherited(decl);
  if (Options.TypeDefinitions) {
    printMembers(decl->getMembers());
  }
}

void PrintAST::visitPatternBindingDecl(PatternBindingDecl *decl) {
  recordDeclLoc(decl);
  if (decl->isStatic())
    printStaticKeyword(decl->getCorrectStaticSpelling());

  // FIXME: We're not printing proper "{ get set }" annotations in pattern
  // binding decls.  As a hack, scan the decl to find out if any of the
  // variables are immutable, and if so, we print as 'let'.  This allows us to
  // handle the 'let x = 4' case properly at least.
  bool isMutable = true;
  decl->getPattern()->forEachVariable([&](VarDecl *V) {
    if (!V->isSettable(V->getDeclContext()))
      isMutable = false;
  });

  printAttributes(decl);
  Printer << (isMutable ? "var " : "let ");
  printPattern(decl->getPattern());
  if (Options.VarInitializers) {
    // FIXME: Implement once we can pretty-print expressions.
  }
}

void PrintAST::visitTopLevelCodeDecl(TopLevelCodeDecl *decl) {
  printBraceStmtElements(decl->getBody(), /*NeedIndent=*/false);
}

void PrintAST::visitIfConfigDecl(IfConfigDecl *ICD) {
  // FIXME: Pretty print #if decls
}

void PrintAST::visitTypeAliasDecl(TypeAliasDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  if (!Options.SkipIntroducerKeywords)
    Printer << "typealias ";
  recordDeclLoc(decl);
  Printer.printName(decl->getName());
  if (Options.TypeDefinitions && decl->hasUnderlyingType()) {
    Type Ty = decl->getUnderlyingType();
    // If the underlying type is private, don't print it.
    if (!Options.SkipPrivateStdlibDecls || !Ty.isPrivateStdlibType()) {
      Printer << " = ";
      Ty.print(Printer, Options);
    }
  }
}

void PrintAST::visitGenericTypeParamDecl(GenericTypeParamDecl *decl) {
  recordDeclLoc(decl);
  Printer.printName(decl->getName());
  printInheritedWithSuperclass(decl);
}

void PrintAST::visitAssociatedTypeDecl(AssociatedTypeDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  if (!Options.SkipIntroducerKeywords)
    Printer << "typealias ";
  recordDeclLoc(decl);
  Printer.printName(decl->getName());
  printInheritedWithSuperclass(decl);

  if (!decl->getDefaultDefinitionLoc().isNull()) {
    Printer << " = ";
    decl->getDefaultDefinitionLoc().getType().print(Printer, Options);
  }
}

void PrintAST::visitEnumDecl(EnumDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  if (!Options.SkipIntroducerKeywords)
    Printer << "enum ";
  recordDeclLoc(decl);
  printNominalDeclName(decl);
  printInherited(decl);
  if (Options.TypeDefinitions) {
    printMembers(decl->getMembers());
  }
}

void PrintAST::visitStructDecl(StructDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  if (!Options.SkipIntroducerKeywords)
    Printer << "struct ";
  recordDeclLoc(decl);
  printNominalDeclName(decl);
  printInherited(decl);
  if (Options.TypeDefinitions) {
    printMembers(decl->getMembers());
  }
}

void PrintAST::visitClassDecl(ClassDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  if (!Options.SkipIntroducerKeywords)
    Printer << "class ";
  recordDeclLoc(decl);
  printNominalDeclName(decl);
  printInheritedWithSuperclass(decl);
  if (Options.TypeDefinitions) {
    printMembers(decl->getMembers());
  }
}

void PrintAST::visitProtocolDecl(ProtocolDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  if (!Options.SkipIntroducerKeywords)
    Printer << "protocol ";
  recordDeclLoc(decl);
  printNominalDeclName(decl);
  printInherited(decl);
  if (Options.TypeDefinitions) {
    printMembers(decl->getMembers());
  }
}

void PrintAST::visitVarDecl(VarDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  printOverrideKeyword(decl);
  if (!Options.SkipIntroducerKeywords) {
    if (decl->isStatic())
      printStaticKeyword(decl->getCorrectStaticSpelling());
    Printer << (decl->isLet() ? "let " : "var ");
  }
  recordDeclLoc(decl);
  Printer.printName(decl->getName());
  if (decl->hasType()) {
    Printer << ": ";
    decl->getType().print(Printer, Options);
  }

  printAccessors(decl);
}

void PrintAST::visitParamDecl(ParamDecl *decl) {
  return visitVarDecl(decl);
}

void PrintAST::printOneParameter(Identifier ArgName,
                                 const Pattern *BodyPattern,
                                 bool ArgNameIsAPIByDefault,
                                 bool StripOuterSliceType,
                                 bool Curried) {
  auto printArgName = [&]() {
    // Print argument name.
    auto BodyName = BodyPattern->getBoundName();
    if (Curried) {
      // For curried parameters, just print the body name if there is one.
      Printer.printName(BodyName);
      Printer << ": ";
    } else {
      auto printArg = [&]{
        if (!ArgName.empty() && !ArgNameIsAPIByDefault)
          Printer << "#";
        Printer.printName(ArgName);
      };

      switch (Options.ArgAndParamPrinting) {
      case PrintOptions::ArgAndParamPrintingMode::ArgumentOnly:
        printArg();
        break;
      case PrintOptions::ArgAndParamPrintingMode::BothIfDifferent:
        if (ArgName == BodyName) {
          printArg();
          break;
        }
        if (ArgName.empty() && !ArgNameIsAPIByDefault) {
          Printer.printName(BodyName);
          break;
        }
        SWIFT_FALLTHROUGH;
      case PrintOptions::ArgAndParamPrintingMode::BothAlways:
        Printer.printName(ArgName);
        Printer << " ";
        Printer.printName(BodyName);
        break;
      }
      Printer << ": ";
    }
  };

  if (auto *VP = dyn_cast<VarPattern>(BodyPattern))
    BodyPattern = VP->getSubPattern();
  auto *TypedBodyPattern = dyn_cast<TypedPattern>(BodyPattern);
  if (!TypedBodyPattern) {
    // It was a syntax error.
    printArgName();
    return;
  }
  auto TheTypeLoc = TypedBodyPattern->getTypeLoc();
  if (TheTypeLoc.hasLocation()) {
    // If the outer typeloc is an InOutTypeRepr, print the 'inout' before the
    // subpattern.
    if (auto *IOTR = dyn_cast<InOutTypeRepr>(TheTypeLoc.getTypeRepr())) {
      TheTypeLoc = TypeLoc(IOTR->getBase());
      if (Type T = TheTypeLoc.getType()) {
        if (auto *IOT = T->getAs<InOutType>()) {
          TheTypeLoc.setType(IOT->getObjectType());
        }
      }

      Printer << "inout ";
    }
  } else {
    if (Type T = TheTypeLoc.getType()) {
      if (auto *IOT = T->getAs<InOutType>()) {
        Printer << "inout ";
        TheTypeLoc.setType(IOT->getObjectType());
      }
    }
  }

  printArgName();

  if (StripOuterSliceType && !TheTypeLoc.hasLocation()) {
    if (auto *BGT = TypedBodyPattern->getType()->getAs<BoundGenericType>()) {
      BGT->getGenericArgs()[0].print(Printer, Options);
      return;
    }
  }
  printTypeLoc(TheTypeLoc);
}

void PrintAST::printFunctionParameters(AbstractFunctionDecl *AFD) {
  ArrayRef<Identifier> ArgNames;
  DeclName Name = AFD->getFullName();
  if (Name) {
    ArgNames = Name.getArgumentNames();
  }

  ArrayRef<Pattern *> BodyPatterns = AFD->getBodyParamPatterns();

  // Skip over the implicit 'self'.
  if (AFD->getImplicitSelfDecl()) {
    BodyPatterns = BodyPatterns.slice(1);
  }

  for (unsigned CurrPattern = 0, NumPatterns = BodyPatterns.size();
       CurrPattern != NumPatterns; ++CurrPattern) {
    bool UseArgName = Name && (CurrPattern == 0);
    if (auto *BodyTuple = dyn_cast<TuplePattern>(BodyPatterns[CurrPattern])) {
      Printer << "(";
      for (unsigned i = 0, e = BodyTuple->getFields().size(); i != e; ++i) {
        if (i > 0)
          Printer << ", ";

        // Determine whether the argument name is API by default.
        bool ArgNameIsAPIByDefault = (CurrPattern == 0 &&
                                      AFD->argumentNameIsAPIByDefault(i)) ||
          (BodyTuple->getFields()[i].getDefaultArgKind() != 
             DefaultArgumentKind::None &&
           Options.PrintDefaultParameterPlaceholder);

        printOneParameter(UseArgName ? ArgNames[i] : Identifier(),
                          BodyTuple->getFields()[i].getPattern(),
                          ArgNameIsAPIByDefault,
                          /*StripOuterSliceType=*/i == e - 1 &&
                            BodyTuple->hasVararg(),
                          /*Curried=*/CurrPattern > 0);
        if (Options.PrintDefaultParameterPlaceholder &&
            BodyTuple->getFields()[i].getDefaultArgKind() !=
                DefaultArgumentKind::None)
          Printer << " = default";
      }
      if (BodyTuple->hasVararg())
        Printer << "...";
      Printer << ")";
      continue;
    }
    bool ArgNameIsAPIByDefault = (CurrPattern == 0 &&
                                  AFD->argumentNameIsAPIByDefault(0));
    auto *BodyParen = cast<ParenPattern>(BodyPatterns[CurrPattern]);
    Printer << "(";
    printOneParameter(UseArgName? ArgNames[0] : Identifier(), 
                      BodyParen->getSubPattern(),
                      ArgNameIsAPIByDefault,
                      /*StripOuterSliceType=*/false,
                      /*Curried=*/CurrPattern > 0);
    Printer << ")";
  }
}

bool PrintAST::printBraceStmtElements(BraceStmt *stmt, bool NeedIndent) {
  IndentRAII IndentMore(*this, NeedIndent);
  bool PrintedSomething = false;
  for (auto element : stmt->getElements()) {
    PrintedSomething = true;
    Printer.printNewline();
    indent();
    if (auto decl = element.dyn_cast<Decl*>()) {
      if (decl->shouldPrintInContext(Options))
        visit(decl);
    } else if (auto stmt = element.dyn_cast<Stmt*>()) {
      visit(stmt);
    } else {
      // FIXME: print expression
      // visit(element.get<Expr*>());
    }
  }
  return PrintedSomething;
}

void PrintAST::visitFuncDecl(FuncDecl *decl) {
  if (decl->isAccessor()) {
    printDocumentationComment(decl);
    printAttributes(decl);
    recordDeclLoc(decl);
    switch (decl->getAccessorKind()) {
    case AccessorKind::NotAccessor: break;
    case AccessorKind::IsGetter:
      Printer << "get {";
      break;
    case AccessorKind::IsDidSet:
      Printer << "didSet {";
      break;
    case AccessorKind::IsSetter:
    case AccessorKind::IsWillSet:
      Printer << (decl->isSetter() ? "set" : "willSet");

      auto BodyParams = decl->getBodyParamPatterns();
      auto ValueParam = BodyParams.back()->getSemanticsProvidingPattern();
      if (auto *TP = dyn_cast<TuplePattern>(ValueParam)) {
        if (!TP->isImplicit() && !TP->getFields().empty()) {
          auto *P = TP->getFields().begin()->getPattern()->
                        getSemanticsProvidingPattern();
          Identifier Name = P->getBoundName();
          if (!Name.empty() && !P->isImplicit()) {
            Printer << "(";
            Printer.printName(Name);
            Printer << ")";
          }
        }
      }
      Printer << " {";
    }
    if (Options.FunctionDefinitions && decl->getBody()) {
      if (printBraceStmtElements(decl->getBody())) {
        Printer.printNewline();
        indent();
      }
    }
    Printer << "}";
  } else {
    printDocumentationComment(decl);
    printAttributes(decl);
    printOverrideKeyword(decl);
    if (decl->isStatic() && !decl->isOperator())
      printStaticKeyword(decl->getCorrectStaticSpelling());
    if (!Options.SkipIntroducerKeywords)
      Printer << "func ";
    recordDeclLoc(decl);
    if (!decl->hasName())
      Printer << "<anonymous>";
    else
      Printer.printName(decl->getName());
    if (decl->isGeneric()) {
      printGenericParams(decl->getGenericParams());
    }

    printFunctionParameters(decl);

    auto &Context = decl->getASTContext();
    Type ResultTy = decl->getResultType();
    if (ResultTy && !ResultTy->isEqual(TupleType::getEmpty(Context))) {
      Printer << " -> ";
      ResultTy->print(Printer, Options);
    }

    if (!Options.FunctionDefinitions || !decl->getBody()) {
      return;
    }

    Printer << " ";
    visit(decl->getBody());
  }
}

void PrintAST::printEnumElement(EnumElementDecl *elt) {
  Printer.printName(elt->getName());

  if (elt->hasArgumentType()) {
    Type Ty = elt->getArgumentType();
    if (!Options.SkipPrivateStdlibDecls || !Ty.isPrivateStdlibType())
      Ty.print(Printer, Options);
  }
}

void PrintAST::visitEnumCaseDecl(EnumCaseDecl *decl) {
  // FIXME: Attributes?
  recordDeclLoc(decl);
  Printer << "case ";

  interleave(decl->getElements().begin(), decl->getElements().end(),
    [&](EnumElementDecl *elt) {
      printEnumElement(elt);
    },
    [&] { Printer << ", "; });
}

void PrintAST::visitEnumElementDecl(EnumElementDecl *decl) {
  if (!decl->shouldPrintInContext(Options))
    return;

  // In cases where there is no parent EnumCaseDecl (such as imported or
  // deserialized elements), print the element independently.
  Printer << "case ";
  printEnumElement(decl);
}

void PrintAST::visitSubscriptDecl(SubscriptDecl *decl) {
  printAttributes(decl);
  printOverrideKeyword(decl);
  recordDeclLoc(decl);
  Printer << "subscript ";
  printPattern(decl->getIndices());
  Printer << " -> ";
  decl->getElementType().print(Printer, Options);

  printAccessors(decl);
}

void PrintAST::visitConstructorDecl(ConstructorDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);

  if (decl->getInitKind() == CtorInitializerKind::Convenience ||
      decl->getInitKind() == CtorInitializerKind::ConvenienceFactory)
    Printer << "convenience ";
  
  recordDeclLoc(decl);
  Printer << "init";
  if (decl->isGeneric())
    printGenericParams(decl->getGenericParams());
  printFunctionParameters(decl);
  
  if (decl->getInitKind() == CtorInitializerKind::Factory) {
    Printer << " -> ";
    Printer.printName(decl->getExtensionType()->getAnyNominal()->getName());
  }

  if (!Options.FunctionDefinitions || !decl->getBody()) {
    return;
  }

  Printer << " ";
  visit(decl->getBody());
}

void PrintAST::visitDestructorDecl(DestructorDecl *decl) {
  printDocumentationComment(decl);
  printAttributes(decl);
  recordDeclLoc(decl);
  Printer << "deinit ";

  if (!Options.FunctionDefinitions || !decl->getBody()) {
    return;
  }

  Printer << " ";
  visit(decl->getBody());
}

void PrintAST::visitInfixOperatorDecl(InfixOperatorDecl *decl) {
  Printer << "operator infix ";
  recordDeclLoc(decl);
  Printer.printName(decl->getName());
  Printer << " {";
  Printer.printNewline();
  {
    IndentRAII indentMore(*this);
    if (decl->getAssociativityLoc().isValid()) {
      indent();
      Printer << "associativity ";
      switch (decl->getAssociativity()) {
      case Associativity::None:
        Printer << "none";
        break;
      case Associativity::Left:
        Printer << "left";
        break;
      case Associativity::Right:
        Printer << "right";
        break;
      }
      Printer.printNewline();
    }
    if (decl->getPrecedenceLoc().isValid()) {
      indent();
      Printer << "precedence " << decl->getPrecedence();
      Printer.printNewline();
    }
  }
  indent();
  Printer << "}";
}

void PrintAST::visitPrefixOperatorDecl(PrefixOperatorDecl *decl) {
  Printer << "operator prefix ";
  recordDeclLoc(decl);
  Printer.printName(decl->getName());
  Printer << " {";
  Printer.printNewline();
  Printer << "}";
}

void PrintAST::visitPostfixOperatorDecl(PostfixOperatorDecl *decl) {
  Printer << "operator postfix ";
  recordDeclLoc(decl);
  Printer.printName(decl->getName());
  Printer << " {";
  Printer.printNewline();
  Printer << "}";
}

void PrintAST::visitBraceStmt(BraceStmt *stmt) {
  Printer << "{";
  printBraceStmtElements(stmt);
  Printer.printNewline();
  indent();
  Printer << "}";
}

void PrintAST::visitReturnStmt(ReturnStmt *stmt) {
  Printer << "return";
  if (stmt->hasResult()) {
    Printer << " ";
    // FIXME: print expression.
  }
}

void PrintAST::visitIfStmt(IfStmt *stmt) {
  Printer << "if ";
  // FIXME: print condition
  Printer << " ";
  visit(stmt->getThenStmt());
  if (auto elseStmt = stmt->getElseStmt()) {
    Printer << " else ";
    visit(elseStmt);
  }
}

void PrintAST::visitIfConfigStmt(IfConfigStmt *stmt) {
  for (auto &Clause : stmt->getClauses()) {
    if (&Clause == &*stmt->getClauses().begin())
      Printer << "#if "; // FIXME: print condition
    else if (Clause.Cond)
      Printer << "#elseif"; // FIXME: print condition
    else
      Printer << "#else";
    Printer.printNewline();
    visit(Clause.Body);
  }
  Printer.printNewline();
  Printer << "#endif";
}

void PrintAST::visitWhileStmt(WhileStmt *stmt) {
  Printer << "while ";
  // FIXME: print condition
  Printer << " ";
  visit(stmt->getBody());
}

void PrintAST::visitDoWhileStmt(DoWhileStmt *stmt) {
  Printer << "do ";
  visit(stmt->getBody());
  Printer << " while ";
  // FIXME: print condition
}

void PrintAST::visitForStmt(ForStmt *stmt) {
  Printer << "for (";
  // FIXME: print initializer
  Printer << "; ";
  if (stmt->getCond().isNonNull()) {
    // FIXME: print cond
  }
  Printer << "; ";
  // FIXME: print increment
  Printer << ") ";
  visit(stmt->getBody());
}

void PrintAST::visitForEachStmt(ForEachStmt *stmt) {
  Printer << "for ";
  printPattern(stmt->getPattern());
  Printer << " in ";
  // FIXME: print container
  Printer << " ";
  visit(stmt->getBody());
}

void PrintAST::visitBreakStmt(BreakStmt *stmt) {
  Printer << "break";
}

void PrintAST::visitContinueStmt(ContinueStmt *stmt) {
  Printer << "continue";
}

void PrintAST::visitFallthroughStmt(FallthroughStmt *stmt) {
  Printer << "fallthrough";
}

void PrintAST::visitSwitchStmt(SwitchStmt *stmt) {
  Printer << "switch ";
  // FIXME: print subject
  Printer << "{";
  Printer.printNewline();
  for (CaseStmt *C : stmt->getCases()) {
    visit(C);
  }
  Printer.printNewline();
  indent();
  Printer << "}";
}

void PrintAST::visitCaseStmt(CaseStmt *CS) {
  if (CS->isDefault()) {
    Printer << "default";
  } else {
    auto PrintCaseLabelItem = [&](const CaseLabelItem &CLI) {
      if (auto *P = CLI.getPattern())
        printPattern(P);
      if (CLI.getGuardExpr()) {
        Printer << " where ";
        // FIXME: print guard expr
      }
    };
    Printer << "case ";
    interleave(CS->getCaseLabelItems(), PrintCaseLabelItem,
               [&] { Printer << ", "; });
  }
  Printer << ":";
  Printer.printNewline();

  printBraceStmtElements(cast<BraceStmt>(CS->getBody()));
}

void Decl::print(raw_ostream &os) const {
  PrintOptions options;
  options.FunctionDefinitions = true;
  options.TypeDefinitions = true;
  options.VarInitializers = true;

  print(os, options);
}

void Decl::print(raw_ostream &OS, const PrintOptions &Opts) const {
  StreamPrinter Printer(OS);
  print(Printer, Opts);
}

bool Decl::print(ASTPrinter &Printer, const PrintOptions &Opts) const {
  PrintAST printer(Printer, Opts);
  return printer.visit(const_cast<Decl *>(this));
}

bool Decl::shouldPrintInContext(const PrintOptions &PO) const {
  // Skip getters/setters. They are part of the variable or subscript.
  if (isa<FuncDecl>(this) && cast<FuncDecl>(this)->isAccessor())
    return false;

  if (PO.ExplodePatternBindingDecls) {
    if (isa<VarDecl>(this))
      return true;
    if (isa<PatternBindingDecl>(this))
      return false;
  } else {
    // Try to preserve the PatternBindingDecl structure.

    // Skip stored variables, unless they came from a Clang module.
    // Stored variables in Swift source will be picked up by the
    // PatternBindingDecl.
    if (auto *VD = dyn_cast<VarDecl>(this)) {
      if (!VD->hasClangNode() && VD->hasStorage() &&
          VD->getStorageKind() != VarDecl::Observing)
        return false;
    }

    // Skip pattern bindings that consist of just one computed variable.
    if (auto pbd = dyn_cast<PatternBindingDecl>(this)) {
      auto pattern = pbd->getPattern()->getSemanticsProvidingPattern();
      if (auto named = dyn_cast<NamedPattern>(pattern)) {
        auto StorageKind = named->getDecl()->getStorageKind();
        if (StorageKind == VarDecl::Computed ||
            StorageKind == VarDecl::Observing)
          return false;
      }
    }
  }

  if (auto EED = dyn_cast<EnumElementDecl>(this)) {
    // Enum elements are printed as part of the EnumCaseDecl, unless they were
    // imported without source info.
    return !EED->getSourceRange().isValid();
  }

  // Print everything else.
  return true;
}

void Pattern::print(llvm::raw_ostream &OS, const PrintOptions &Options) const {
  StreamPrinter StreamPrinter(OS);
  PrintAST Printer(StreamPrinter, Options);
  Printer.printPattern(this);
}

//===----------------------------------------------------------------------===//
//  Type Printing
//===----------------------------------------------------------------------===//

namespace {
class TypePrinter : public TypeVisitor<TypePrinter> {
  ASTPrinter &Printer;
  const PrintOptions &Options;
  Optional<std::vector<GenericParamList *>> UnwrappedGenericParams;

  void printDeclContext(DeclContext *DC) {
    switch (DC->getContextKind()) {
    case DeclContextKind::Module: {
      Module *M = cast<Module>(DC);

      if (auto Parent = M->getParent())
        printDeclContext(Parent);
      Printer.printModuleRef(M, M->Name);
      return;
    }

    case DeclContextKind::FileUnit:
      printDeclContext(DC->getParent());
      return;

    case DeclContextKind::AbstractClosureExpr:
      // FIXME: print closures somehow.
      return;

    case DeclContextKind::NominalTypeDecl:
      visit(cast<NominalTypeDecl>(DC)->getType());
      return;

    case DeclContextKind::ExtensionDecl:
      visit(cast<ExtensionDecl>(DC)->getExtendedType());
      return;

    case DeclContextKind::Initializer:
    case DeclContextKind::TopLevelCodeDecl:
      llvm_unreachable("bad decl context");

    case DeclContextKind::AbstractFunctionDecl:
      visit(cast<AbstractFunctionDecl>(DC)->getType());
      return;
    }
  }

  void printGenericArgs(ArrayRef<Type> Args) {
    if (Args.empty())
      return;

    Printer << "<";
    bool First = true;
    for (Type Arg : Args) {
      if (First)
        First = false;
      else
        Printer << ", ";
      visit(Arg);
    }
    Printer << ">";
  }

  static bool isSimple(Type type) {
    switch (type->getKind()) {
    case TypeKind::Function:
    case TypeKind::PolymorphicFunction:
    case TypeKind::GenericFunction:
      return false;

    case TypeKind::Metatype:
    case TypeKind::ExistentialMetatype:
      return !cast<AnyMetatypeType>(type.getPointer())->hasRepresentation();

    default:
      return true;
    }
  }

  /// Helper function for printing a type that is embedded within a larger type.
  ///
  /// This is necessary whenever the inner type may not normally be represented
  /// as a 'type-simple' production in the type grammar.
  void printWithParensIfNotSimple(Type T) {
    if (T.isNull()) {
      visit(T);
      return;
    }

    if (!isSimple(T)) {
      Printer << "(";
      visit(T);
      Printer << ")";
    } else {
      visit(T);
    }
  }

  void printGenericParams(GenericParamList *Params) {
    PrintAST(Printer, Options).printGenericParams(Params);
  }

  template <typename T>
  void printModuleContext(T *Ty) {
    Module *Mod = Ty->getDecl()->getModuleContext();
    Printer.printModuleRef(Mod, Mod->Name);
    Printer << ".";
  }

  template <typename T>
  void printTypeDeclName(T *Ty) {
    TypeDecl *TD = Ty->getDecl();
    Printer.printTypeRef(TD, TD->getName());
  }

  // FIXME: we should have a callback that would tell us
  // whether it's kosher to print a module name or not
  bool isLLDBExpressionModule(Module *M) {
    if (!M)
      return false;
    return M->Name.str().startswith("lldb_expr_");
  }

  bool shouldPrintFullyQualified(TypeBase *T) {
    if (Options.FullyQualifiedTypes)
      return true;

    if (!Options.FullyQualifiedTypesIfAmbiguous)
      return false;

    Decl *D = nullptr;
    if (auto *NAT = dyn_cast<NameAliasType>(T))
      D = NAT->getDecl();
    else
      D = T->getAnyNominal();

    // If we can not find the declaration, be extra careful and print
    // the type qualified.
    if (!D)
      return true;

    Module *M = D->getDeclContext()->getParentModule();

    // Don't print qualifiers for types from the standard library.
    if (M->isStdlibModule() ||
        M->Name == T->getASTContext().ObjCModuleName ||
        M->isSystemModule() ||
        isLLDBExpressionModule(M))
      return false;

    // Don't print qualifiers for imported types.
    for (auto File : M->getFiles()) {
      if (File->getKind() == FileUnitKind::ClangModule)
        return false;
    }

    return true;
  }

public:
  TypePrinter(ASTPrinter &Printer, const PrintOptions &PO)
      : Printer(Printer), Options(PO) {}

  void visitErrorType(ErrorType *T) {
    Printer << "<<error type>>";
  }

  void visitBuiltinRawPointerType(BuiltinRawPointerType *T) {
    Printer << "Builtin.RawPointer";
  }

  void visitBuiltinNativeObjectType(BuiltinNativeObjectType *T) {
    Printer << "Builtin.NativeObject";
  }

  void visitBuiltinUnknownObjectType(BuiltinUnknownObjectType *T) {
    Printer << "Builtin.UnknownObject";
  }

  void visitBuiltinVectorType(BuiltinVectorType *T) {
    llvm::SmallString<32> UnderlyingStrVec;
    StringRef UnderlyingStr;
    {
      // FIXME: Ugly hack: remove the .Builtin from the element type.
      llvm::raw_svector_ostream UnderlyingOS(UnderlyingStrVec);
      visit(T->getElementType());
      if (UnderlyingStrVec.startswith("Builtin."))
        UnderlyingStr = UnderlyingStrVec.substr(9);
      else
        UnderlyingStr = UnderlyingStrVec;
    }

    Printer << "Builtin.Vec" << T->getNumElements() << "x" << UnderlyingStr;
  }

  void visitBuiltinIntegerType(BuiltinIntegerType *T) {
    auto width = T->getWidth();
    if (width.isFixedWidth()) {
      Printer << "Builtin.Int" << width.getFixedWidth();
    } else if (width.isPointerWidth()) {
      Printer << "Builtin.Word";
    } else {
      llvm_unreachable("impossible bit width");
    }
  }

  void visitBuiltinFloatType(BuiltinFloatType *T) {
    switch (T->getFPKind()) {
    case BuiltinFloatType::IEEE16:  Printer << "Builtin.FPIEEE16"; return;
    case BuiltinFloatType::IEEE32:  Printer << "Builtin.FPIEEE32"; return;
    case BuiltinFloatType::IEEE64:  Printer << "Builtin.FPIEEE64"; return;
    case BuiltinFloatType::IEEE80:  Printer << "Builtin.FPIEEE80"; return;
    case BuiltinFloatType::IEEE128: Printer << "Builtin.FPIEEE128"; return;
    case BuiltinFloatType::PPC128:  Printer << "Builtin.FPPPC128"; return;
    }
  }

  void visitNameAliasType(NameAliasType *T) {
    if (shouldPrintFullyQualified(T)) {
      if (auto ParentDC = T->getDecl()->getDeclContext()) {
        printDeclContext(ParentDC);
        Printer << ".";
      }
    }
    printTypeDeclName(T);
  }

  void visitParenType(ParenType *T) {
    Printer << "(";
    visit(T->getUnderlyingType());
    Printer << ")";
  }

  void visitTupleType(TupleType *T) {
    Printer << "(";

    auto Fields = T->getFields();
    for (unsigned i = 0, e = Fields.size(); i != e; ++i) {
      if (i)
        Printer << ", ";
      const TupleTypeElt &TD = Fields[i];
      Type EltType = TD.getType();
      if (auto *IOT = EltType->getAs<InOutType>()) {
        Printer << "inout ";
        EltType = IOT->getObjectType();
      }

      if (TD.hasName()) {
        Printer.printName(TD.getName());
        Printer << ": ";
      }

      if (TD.isVararg()) {
        visit(TD.getVarargBaseTy());
        Printer << "...";
      } else
        visit(EltType);
    }
    Printer << ")";
  }

  void visitUnboundGenericType(UnboundGenericType *T) {
    if (auto ParentType = T->getParent()) {
      visit(ParentType);
      Printer << ".";
    } else if (shouldPrintFullyQualified(T)) {
      printModuleContext(T);
    }
    printTypeDeclName(T);
  }

  void visitBoundGenericType(BoundGenericType *T) {
    if (Options.SynthesizeSugarOnTypes) {
      auto *NT = T->getDecl();
      auto &Ctx = T->getASTContext();
      if (NT == Ctx.getArrayDecl()) {
        printWithParensIfNotSimple(T->getGenericArgs()[0]);
        Printer << "[]";
        return;
      }
      if (NT == Ctx.getOptionalDecl()) {
        printWithParensIfNotSimple(T->getGenericArgs()[0]);
        Printer << "?";
        return;
      }
      if (NT == Ctx.getImplicitlyUnwrappedOptionalDecl()) {
        printWithParensIfNotSimple(T->getGenericArgs()[0]);
        Printer << "!";
        return;
      }
    }
    if (auto ParentType = T->getParent()) {
      visit(ParentType);
      Printer << ".";
    } else if (shouldPrintFullyQualified(T)) {
      printModuleContext(T);
    }

    printTypeDeclName(T);
    printGenericArgs(T->getGenericArgs());
  }

  void visitEnumType(EnumType *T) {
    if (auto ParentType = T->getParent()) {
      visit(ParentType);
      Printer << ".";
    } else if (shouldPrintFullyQualified(T)) {
      printModuleContext(T);
    }

    printTypeDeclName(T);
  }

  void visitStructType(StructType *T) {
    if (auto ParentType = T->getParent()) {
      visit(ParentType);
      Printer << ".";
    } else if (shouldPrintFullyQualified(T)) {
      printModuleContext(T);
    }

    printTypeDeclName(T);
  }

  void visitClassType(ClassType *T) {
    if (auto ParentType = T->getParent()) {
      visit(ParentType);
      Printer << ".";
    } else if (shouldPrintFullyQualified(T)) {
      printModuleContext(T);
    }

    printTypeDeclName(T);
  }

  void visitAnyMetatypeType(AnyMetatypeType *T) {
    if (T->hasRepresentation()) {
      switch (T->getRepresentation()) {
      case MetatypeRepresentation::Thin:  Printer << "@thin ";  break;
      case MetatypeRepresentation::Thick: Printer << "@thick "; break;
      case MetatypeRepresentation::ObjC:  Printer << "@objc_metatype "; break;
      }
    }
    printWithParensIfNotSimple(T->getInstanceType());

    // We spell normal metatypes of existential types as .Protocol.
    if (isa<MetatypeType>(T) && T->getInstanceType()->isAnyExistentialType()) {
      Printer << ".Protocol";
    } else {
      Printer << ".Type";
    }
  }

  void visitModuleType(ModuleType *T) {
    Printer << "module<";
    Printer.printModuleRef(T->getModule(), T->getModule()->Name);
    Printer << ">";
  }

  void visitDynamicSelfType(DynamicSelfType *T) {
    Printer << "Self";
  }

  void printFunctionExtInfo(AnyFunctionType::ExtInfo info) {
    if (info.isAutoClosure())
      Printer << "@auto_closure ";
    switch (info.getCC()) {
    case AbstractCC::Freestanding: break;
    case AbstractCC::Method:
      Printer << "@cc(method) ";
      break;
    case AbstractCC::C:
      Printer << "@cc(cdecl) ";
      break;
    case AbstractCC::ObjCMethod:
      Printer << "@cc(objc_method) ";
      break;
    case AbstractCC::WitnessMethod:
      Printer << "@cc(witness_method) ";
      break;
    }

    if (Options.PrintFunctionRepresentationAttrs) {
      switch (info.getRepresentation()) {
      case AnyFunctionType::Representation::Thick:
        break;
      case AnyFunctionType::Representation::Thin:
        Printer << "@thin ";
        break;
      case AnyFunctionType::Representation::Block:
        Printer << "@objc_block ";
        break;
      }
    }

    if (info.isNoReturn())
      Printer << "@noreturn ";
  }

  void visitFunctionType(FunctionType *T) {
    printFunctionExtInfo(T->getExtInfo());
    printWithParensIfNotSimple(T->getInput());
    Printer << " -> ";
    T->getResult().print(Printer, Options);
  }

  void visitPolymorphicFunctionType(PolymorphicFunctionType *T) {
    printFunctionExtInfo(T->getExtInfo());
    printGenericParams(&T->getGenericParams());
    Printer << " ";
    printWithParensIfNotSimple(T->getInput());
    Printer << " -> ";
    T->getResult().print(Printer, Options);
  }

  void printGenericSignature(ArrayRef<GenericTypeParamType *> genericParams,
                             ArrayRef<Requirement> requirements) {
    // Print the generic parameters.
    Printer << "<";
    bool isFirstParam = true;
    for (auto param : genericParams) {
      if (isFirstParam)
        isFirstParam = false;
      else
        Printer << ", ";

      visit(param);
    }

    // Print the requirements.
    bool isFirstReq = true;
    for (const auto &req : requirements) {
      if (req.getKind() == RequirementKind::WitnessMarker)
        continue;

      if (isFirstReq) {
        Printer << " where ";
        isFirstReq = false;
      } else {
        Printer << ", ";
      }

      visit(req.getFirstType());
      switch (req.getKind()) {
      case RequirementKind::Conformance:
        Printer << " : ";
        break;

      case RequirementKind::SameType:
        Printer << " == ";
        break;

      case RequirementKind::WitnessMarker:
        llvm_unreachable("Handled above");
      }
      visit(req.getSecondType());
    }
    Printer << ">";
  }

  void visitGenericFunctionType(GenericFunctionType *T) {
    printFunctionExtInfo(T->getExtInfo());
    printGenericSignature(T->getGenericParams(), T->getRequirements());
    Printer << " ";
    printWithParensIfNotSimple(T->getInput());

    Printer << " -> ";
    T->getResult().print(Printer, Options);
  }

  void printCalleeConvention(ParameterConvention conv) {
    switch (conv) {
    case ParameterConvention::Direct_Unowned:
      return;
    case ParameterConvention::Direct_Owned:
      Printer << "@callee_owned ";
      return;
    case ParameterConvention::Direct_Guaranteed:
      Printer << "@callee_guaranteed ";
      return;
    case ParameterConvention::Indirect_In:
    case ParameterConvention::Indirect_Out:
    case ParameterConvention::Indirect_Inout:
      llvm_unreachable("callee convention cannot be indirect");
    }
    llvm_unreachable("bad convention");
  }

  void visitSILFunctionType(SILFunctionType *T) {
    printFunctionExtInfo(T->getExtInfo());
    printCalleeConvention(T->getCalleeConvention());
    if (auto sig = T->getGenericSignature()) {
      printGenericSignature(sig->getGenericParams(), sig->getRequirements());
      Printer << " ";
    }
    Printer << "(";
    bool first = true;
    for (auto param : T->getInterfaceParameters()) {
      if (first) {
        first = false;
      } else {
        Printer << ", ";
      }
      param.print(Printer, Options);
    }
    Printer << ") -> ";

    T->getInterfaceResult().print(Printer, Options);
  }
  
  void visitSILBlockStorageType(SILBlockStorageType *T) {
    Printer << "@block_storage ";
    printWithParensIfNotSimple(T->getCaptureType());
  }

  void visitArraySliceType(ArraySliceType *T) {
    printWithParensIfNotSimple(T->getBaseType());
    Printer << "[]";
  }

  void visitOptionalType(OptionalType *T) {
    printWithParensIfNotSimple(T->getBaseType());
    Printer << "?";
  }

  void visitImplicitlyUnwrappedOptionalType(ImplicitlyUnwrappedOptionalType *T) {
    printWithParensIfNotSimple(T->getBaseType());
    Printer <<  "!";
  }

  void visitProtocolType(ProtocolType *T) {
    printTypeDeclName(T);
  }

  void visitProtocolCompositionType(ProtocolCompositionType *T) {
    Printer << "protocol<";
    bool First = true;
    for (auto Proto : T->getProtocols()) {
      if (First)
        First = false;
      else
        Printer << ", ";
      visit(Proto);
    }
    Printer << ">";
  }

  void visitLValueType(LValueType *T) {
    Printer << "@lvalue ";
    visit(T->getObjectType());
  }

  void visitInOutType(InOutType *T) {
    Printer << "inout ";
    visit(T->getObjectType());
  }

  void visitArchetypeType(ArchetypeType *T) {
    if (auto existentialTy = T->getOpenedExistentialType()) {
      Printer << "@opened(" << T->getOpenedExistentialID() << ") ";
      visit(existentialTy);
    } else {
      Printer << T->getFullName();
    }
  }

  GenericParamList *getGenericParamListAtDepth(unsigned depth) {
    assert(Options.ContextGenericParams);
    auto &paramLists = UnwrappedGenericParams.cache([&]{
      std::vector<GenericParamList *> paramLists;
      for (auto *params = Options.ContextGenericParams;
           params;
           params = params->getOuterParameters()) {
        paramLists.push_back(params);
      }
      return paramLists;
    });
    return paramLists.rbegin()[depth];
  }

  void visitGenericTypeParamType(GenericTypeParamType *T) {
    // Substitute a context archetype if we have context generic params.
    if (Options.ContextGenericParams) {
      return visit(getGenericParamListAtDepth(T->getDepth())
                     ->getPrimaryArchetypes()[T->getIndex()]);
    }

    auto Name = T->getName();
    if (Name.empty())
      Printer << "<anonymous>";
    else
      Printer.printName(Name);
  }

  void visitAssociatedTypeType(AssociatedTypeType *T) {
    auto Name = T->getDecl()->getName();
    if (Name.empty())
      Printer << "<anonymous>";
    else
      Printer.printName(Name);
  }

  void visitSubstitutedType(SubstitutedType *T) {
    visit(T->getReplacementType());
  }

  void visitDependentMemberType(DependentMemberType *T) {
    visit(T->getBase());
    Printer << ".";
    Printer.printName(T->getName());
  }

  void visitUnownedStorageType(UnownedStorageType *T) {
    Printer << "@sil_unowned ";
    visit(T->getReferentType());
  }

  void visitUnmanagedStorageType(UnmanagedStorageType *T) {
    Printer << "@sil_unmanaged ";
    visit(T->getReferentType());
  }

  void visitWeakStorageType(WeakStorageType *T) {
    Printer << "@sil_weak ";
    visit(T->getReferentType());
  }

  void visitTypeVariableType(TypeVariableType *T) {
    auto Base = T->getBaseBeingSubstituted();
    
    if (T->getASTContext().LangOpts.DebugConstraintSolver ||
        (T->isEqual(Base)) ||
        T->isPrinting) {
      Printer << "$T" << T->getID();
      return;
    }
    
    llvm::SaveAndRestore<bool> isPrinting(T->isPrinting, true);
    
    visit(Base);
  }
};
} // unnamed namespace

void Type::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}
void Type::print(raw_ostream &OS, const PrintOptions &PO) const {
  StreamPrinter Printer(OS);
  print(Printer, PO);
}
void Type::print(ASTPrinter &Printer, const PrintOptions &PO) const {
  if (isNull())
    Printer << "<null>";
  else
    TypePrinter(Printer, PO).visit(*this);
}

void GenericSignature::print(raw_ostream &OS) const {
  StreamPrinter Printer(OS);
  TypePrinter(Printer, PrintOptions())
    .printGenericSignature(getGenericParams(), getRequirements());
}
void GenericSignature::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}

static StringRef getStringForParameterConvention(ParameterConvention conv) {
  switch (conv) {
  case ParameterConvention::Indirect_In: return "@in ";
  case ParameterConvention::Indirect_Out: return "@out ";
  case ParameterConvention::Indirect_Inout: return "@inout ";
  case ParameterConvention::Direct_Owned: return "@owned ";
  case ParameterConvention::Direct_Unowned: return "";
  case ParameterConvention::Direct_Guaranteed: return "@guaranteed ";
  }
  llvm_unreachable("bad parameter convention");
}

StringRef swift::getCheckedCastKindName(CheckedCastKind kind) {
  switch (kind) {
  case CheckedCastKind::Unresolved:
    return "unresolved";
  case CheckedCastKind::Coercion:
    return "coercion";
  case CheckedCastKind::Downcast:
    return "downcast";
  case CheckedCastKind::SuperToArchetype:
    return "super_to_archetype";
  case CheckedCastKind::ArchetypeToArchetype:
    return "archetype_to_archetype";
  case CheckedCastKind::ArchetypeToConcrete:
    return "archetype_to_concrete";
  case CheckedCastKind::ExistentialToArchetype:
    return "existential_to_archetype";
  case CheckedCastKind::ExistentialToConcrete:
    return "existential_to_concrete";
  case CheckedCastKind::ConcreteToArchetype:
    return "concrete_to_archetype";
  case CheckedCastKind::ConcreteToUnrelatedExistential:
    return "concrete_to_unrelated_existential";
  case CheckedCastKind::ArrayDowncast:
    return "array_downcast";
  }
  llvm_unreachable("bad checked cast name");
}

void SILParameterInfo::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}
void SILParameterInfo::print(raw_ostream &OS, const PrintOptions &Opts) const {
  StreamPrinter Printer(OS);
  print(Printer, Opts);
}
void SILParameterInfo::print(ASTPrinter &Printer,
                             const PrintOptions &Opts) const {
  Printer << getStringForParameterConvention(getConvention());
  getType().print(Printer, Opts);
}

static StringRef getStringForResultConvention(ResultConvention conv) {
  switch (conv) {
  case ResultConvention::Owned: return "@owned ";
  case ResultConvention::Unowned: return "";
  case ResultConvention::UnownedInnerPointer: return "@unowned_inner_pointer ";
  case ResultConvention::Autoreleased: return "@autoreleased ";
  }
  llvm_unreachable("bad result convention");
}

void SILResultInfo::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}
void SILResultInfo::print(raw_ostream &OS, const PrintOptions &Opts) const {
  StreamPrinter Printer(OS);
  print(Printer, Opts);
}
void SILResultInfo::print(ASTPrinter &Printer, const PrintOptions &Opts) const {
  Printer << getStringForResultConvention(getConvention());
  getType().print(Printer, Opts);
}

std::string Type::getString(const PrintOptions &PO) const {
  std::string Result;
  llvm::raw_string_ostream OS(Result);
  print(OS, PO);
  return OS.str();
}

std::string TypeBase::getString(const PrintOptions &PO) const {
  std::string Result;
  llvm::raw_string_ostream OS(Result);
  print(OS, PO);
  return OS.str();
}

void TypeBase::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}

void TypeBase::print(raw_ostream &OS, const PrintOptions &PO) const {
  Type(const_cast<TypeBase *>(this)).print(OS, PO);
}
void TypeBase::print(ASTPrinter &Printer, const PrintOptions &PO) const {
  Type(const_cast<TypeBase *>(this)).print(Printer, PO);
}


