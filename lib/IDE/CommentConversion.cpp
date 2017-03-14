//===--- CommentConversion.cpp - Conversion of comments to other formats --===//
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

#include "swift/IDE/CommentConversion.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Comment.h"
#include "swift/AST/Decl.h"
#include "swift/AST/USRGeneration.h"
#include "swift/AST/RawComment.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Markup/Markup.h"
#include "swift/Markup/XMLUtils.h"
#include "swift/Parse/Token.h"
#include "swift/Subsystems.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/Index/CommentToXML.h"

using namespace swift::markup;
using namespace swift;

//===----------------------------------------------------------------------===//
// Conversion to XML.
//===----------------------------------------------------------------------===//

namespace {
struct CommentToXMLConverter {
  raw_ostream &OS;

  CommentToXMLConverter(raw_ostream &OS) : OS(OS) {}

  void printRawHTML(StringRef Tag) {
    OS << "<rawHTML>";
    appendWithCDATAEscaping(OS, Tag);
    OS << "</rawHTML>";
  }

  void printASTNode(const MarkupASTNode *N) {
    switch (N->getKind()) {
#define MARKUP_AST_NODE(Id, Parent) \
    case ASTNodeKind::Id: \
      print##Id(cast<Id>(N)); \
      break;
#define ABSTRACT_MARKUP_AST_NODE(Id, Parent)
#define MARKUP_AST_NODE_RANGE(Id, FirstId, LastId)
#include "swift/Markup/ASTNodes.def"
    }
  }

#define MARKUP_SIMPLE_FIELD(Id, Keyword, XMLKind) \
  void print##Id(const Id *Field) { \
    OS << "<" #XMLKind << ">"; \
    for (auto Child : Field->getChildren()) \
      printASTNode(Child); \
\
    OS << "</" #XMLKind << ">"; \
}
#include "swift/Markup/SimpleFields.def"

  void printDocument(const Document *D) {
    llvm_unreachable("Can't print a swift::markup::Document as XML directly");
  }

  void printBlockQuote(const BlockQuote *BQ) {
    for (const auto *N : BQ->getChildren())
      printASTNode(N);
  }

  void printList(const List *L) {
    OS << (L->isOrdered() ? "<List-Number>" : "<List-Bullet>");
    for (const auto *N : L->getChildren())
      printASTNode(N);

    OS << (L->isOrdered() ? "</List-Number>" : "</List-Bullet>");
  }

  void printItem(const Item *I) {
    OS << "<Item>";
    for (const auto *N : I->getChildren())
      printASTNode(N);

    OS << "</Item>";
  }

  void printCode(const Code *C) {
    OS << "<codeVoice>";
    appendWithXMLEscaping(OS, C->getLiteralContent());
    OS << "</codeVoice>";
  }

  void printCodeBlock(const CodeBlock *CB) {
    OS << "<CodeListing language=\"";
    appendWithXMLEscaping(OS, CB->getLanguage());
    OS << "\">";
    SmallVector<StringRef, 16> CodeLines;
    CB->getLiteralContent().split(CodeLines, "\n");
    for (auto Line : CodeLines) {
      OS << "<zCodeLineNumbered>";
      appendWithCDATAEscaping(OS, Line);
      OS << "</zCodeLineNumbered>";
    }
    OS << "</CodeListing>";
  }

  void printParagraph(const Paragraph *P) {
    OS << "<Para>";
    for (const auto *N : P->getChildren())
      printASTNode(N);

    OS << "</Para>";
  }

  void printHeader(const Header *H) {
    llvm::SmallString<4> Tag;
    llvm::raw_svector_ostream TagStream(Tag);
    TagStream << "<h" << H->getLevel() << ">";
    printRawHTML(TagStream.str());
    for (auto Child : H->getChildren())
      printASTNode(Child);

    llvm::SmallString<5> EndTag;
    llvm::raw_svector_ostream EndTagStream(EndTag);
    EndTagStream << "</h" << H->getLevel() << ">";
    printRawHTML(EndTagStream.str());
  }

  void printHRule(const HRule *HR) {
    printRawHTML("<hr/>");
  }

  void printText(const Text *T) {
    appendWithXMLEscaping(OS, T->getLiteralContent());
  }

  void printHTML(const HTML *H) {
    printRawHTML(H->getLiteralContent());
  }

  void printInlineHTML(const InlineHTML *IH) {
    printRawHTML(IH->getLiteralContent());
  }

  void printEmphasis(const Emphasis *E) {
    OS << "<emphasis>";
    for (const auto *IC : E->getChildren())
      printASTNode(IC);

    OS << "</emphasis>";
  }

  void printStrong(const Strong *S) {
    OS << "<bold>";
    for (const auto *N : S->getChildren())
      printASTNode(N);

    OS << "</bold>";
  }

  void printLink(const Link *L) {
    SmallString<32> Tag;
    llvm::raw_svector_ostream S(Tag);
    S << "<Link href=\"";
    appendWithXMLEscaping(S, L->getDestination());
    S << "\">";

    OS << S.str();

    for (const auto N : L->getChildren())
      printASTNode(N);

    OS << "</Link>";
  }

  void printSoftBreak(const SoftBreak *SB) {
    OS << " ";
  }

  void printLineBreak(const LineBreak *LB) {
    printRawHTML("<br/>");
  }

  void printPrivateExtension(const PrivateExtension *PE) {
    llvm_unreachable("Can't directly print a Swift Markup PrivateExtension");
  }

  void printImage(const Image *I) {
    SmallString<64> Tag;
    llvm::raw_svector_ostream S(Tag);
    S << "<img src=\"" << I->getDestination() << "\"";
    if (I->hasTitle())
      S << " title=\"" << I->getTitle() << "\"";
    if (I->getChildren().size()) {
      S << " alt=\"";
      for (const auto N : I->getChildren())
        printInlinesUnder(N, S);
      S << "\"";
    }
    S << "\\>";
    printRawHTML(S.str());
  }

  void printParamField(const ParamField *PF) {
    OS << "<Parameter>";
    OS << "<Name>";
    OS << PF->getName();
    OS << "</Name>";
    OS << "<Direction isExplicit=\"0\">in</Direction>";

    if (PF->isClosureParameter()) {
      OS << "<ClosureParameter>";
      visitCommentParts(PF->getParts().getValue());
      OS << "</ClosureParameter>";
    } else {
      OS << "<Discussion>";
      for (auto Child : PF->getChildren()) {
        printASTNode(Child);
      }
      OS << "</Discussion>";
    }
    OS << "</Parameter>";
  }

  void printResultDiscussion(const ReturnsField *RF) {
    OS << "<ResultDiscussion>";
    for (auto Child : RF->getChildren())
      printASTNode(Child);
    OS << "</ResultDiscussion>";
  }

  void printThrowsDiscussion(const ThrowsField *RF) {
    OS << "<ThrowsDiscussion>";
    for (auto Child : RF->getChildren())
      printASTNode(Child);
    OS << "</ThrowsDiscussion>";
  }

  void visitDocComment(const DocComment *DC);
  void visitCommentParts(const swift::markup::CommentParts &Parts);
};
} // unnamed namespace

void CommentToXMLConverter::visitCommentParts(const swift::markup::CommentParts &Parts) {
  if (Parts.Brief.hasValue()) {
    OS << "<Abstract>";
    printASTNode(Parts.Brief.getValue());
    OS << "</Abstract>";
  }

  if (!Parts.ParamFields.empty()) {
    OS << "<Parameters>";
    for (const auto *PF : Parts.ParamFields)
      printParamField(PF);

    OS << "</Parameters>";
  }

  if (Parts.ReturnsField.hasValue())
    printResultDiscussion(Parts.ReturnsField.getValue());

  if (Parts.ThrowsField.hasValue())
    printThrowsDiscussion(Parts.ThrowsField.getValue());

  if (!Parts.BodyNodes.empty()) {
    OS << "<Discussion>";
    for (const auto *N : Parts.BodyNodes)
      printASTNode(N);

    OS << "</Discussion>";
  }
}

void CommentToXMLConverter::visitDocComment(const DocComment *DC) {
  const Decl *D = DC->getDecl();

  StringRef RootEndTag;
  if (isa<AbstractFunctionDecl>(D)) {
    OS << "<Function";
    RootEndTag = "</Function>";
  } else if (isa<StructDecl>(D) || isa<ClassDecl>(D) || isa<ProtocolDecl>(D)) {
    OS << "<Class";
    RootEndTag = "</Class>";
  } else {
    OS << "<Other";
    RootEndTag = "</Other>";
  }

  {
    // Print line and column number.
    auto Loc = D->getLoc();
    if (Loc.isValid()) {
      const auto &SM = D->getASTContext().SourceMgr;
      unsigned BufferID = SM.findBufferContainingLoc(Loc);
      StringRef FileName = SM.getIdentifierForBuffer(BufferID);
      auto LineAndColumn = SM.getLineAndColumn(Loc);
      OS << " file=\"";
      appendWithXMLEscaping(OS, FileName);
      OS << "\"";
      OS << " line=\"" << LineAndColumn.first << "\" column=\""
         << LineAndColumn.second << "\"";
    }
  }

  // Finish the root tag.
  OS << ">";

  auto *VD = dyn_cast<ValueDecl>(D);

  OS << "<Name>";
  if (VD && VD->hasName()) {
    llvm::SmallString<64> SS;
    llvm::raw_svector_ostream NameOS(SS);
    NameOS << VD->getFullName();
    appendWithXMLEscaping(OS, NameOS.str());
  }
  OS << "</Name>";

  if (VD) {
    llvm::SmallString<64> SS;
    bool Failed;
    {
      llvm::raw_svector_ostream OS(SS);
      Failed = ide::printDeclUSR(VD, OS);
    }
    if (!Failed && !SS.empty()) {
      OS << "<USR>" << SS << "</USR>";
    }
  }

  {
    PrintOptions PO = PrintOptions::printInterface();
    PO.PrintAccessibility = false;
    PO.AccessibilityFilter = Accessibility::Private;
    PO.PrintDocumentationComments = false;
    PO.TypeDefinitions = false;
    PO.VarInitializers = false;

    OS << "<Declaration>";
    llvm::SmallString<32> DeclSS;
    {
      llvm::raw_svector_ostream DeclOS(DeclSS);
      D->print(DeclOS, PO);
    }
    appendWithXMLEscaping(OS, DeclSS);
    OS << "</Declaration>";
  }

  visitCommentParts(DC->getParts());

  OS << RootEndTag;
}

static bool getClangDocumentationCommentAsXML(const clang::Decl *D,
                                              raw_ostream &OS) {
  const auto &ClangContext = D->getASTContext();
  const clang::comments::FullComment *FC =
      ClangContext.getCommentForDecl(D, /*PP=*/nullptr);
  if (!FC)
    return false;

  // FIXME: hang the converter object somewhere so that it is persistent
  // between requests to this AST.
  clang::index::CommentToXMLConverter Converter;

  llvm::SmallString<1024> XML;
  Converter.convertCommentToXML(FC, XML, ClangContext);
  OS << XML;
  return true;
}

static void replaceObjcDeclarationsWithSwiftOnes(const Decl *D,
                                                       StringRef Doc,
                                                       raw_ostream &OS) {
  StringRef Open = "<Declaration>";
  StringRef Close = "</Declaration>";
  PrintOptions Options = PrintOptions::printQuickHelpDeclaration();
  std::string S;
  llvm::raw_string_ostream SS(S);
  D->print(SS, Options);
  std::string Signature = SS.str();
  auto OI = Doc.find(Open);
  auto CI = Doc.find(Close);
  if (StringRef::npos != OI && StringRef::npos != CI && CI > OI)
    OS << Doc.substr(0, OI) << Open << Signature << Close <<
      Doc.substr(CI + Close.size());
  else
    OS << Doc;
}

static LineList getLineListFromComment(const StringRef Text) {
  LangOptions LangOpts;
  SourceManager SourceMgr;
  auto Tokens = swift::tokenize(LangOpts, SourceMgr,
                                SourceMgr.addMemBufferCopy(Text));
  std::vector<SingleRawComment> Comments;
  Comments.reserve(Tokens.size());
  for (auto &Tok : Tokens) {
    if (Tok.is(tok::comment)) {
      Comments.push_back(SingleRawComment(Tok.getText(), 0));
    }
  }
  if (Comments.empty())
    return {};

  RawComment Comment(Comments);
  swift::markup::MarkupContext MC;
  return MC.getLineList(Comment);
}

std::string ide::extractPlainTextFromComment(const StringRef Text) {
  return getLineListFromComment(Text).str();
}

bool ide::getDocumentationCommentAsXML(const Decl *D, raw_ostream &OS) {
  auto MaybeClangNode = D->getClangNode();
  if (MaybeClangNode) {
    if (auto *CD = MaybeClangNode.getAsDecl()) {
      std::string S;
      llvm::raw_string_ostream SS(S);
      if (getClangDocumentationCommentAsXML(CD, SS)) {
        replaceObjcDeclarationsWithSwiftOnes(D, SS.str(), OS);
        return true;
      }
    }
    return false;
  }

  swift::markup::MarkupContext MC;
  auto DC = getCascadingDocComment(MC, D);
  if (!DC.hasValue())
    return false;

  CommentToXMLConverter Converter(OS);
  Converter.visitDocComment(DC.getValue());

  OS.flush();
  return true;
}

bool ide::getLocalizationKey(const Decl *D, raw_ostream &OS) {
  swift::markup::MarkupContext MC;
  auto DC = getCascadingDocComment(MC, D);
  if (!DC.hasValue())
    return false;

  if (const auto LKF = DC.getValue()->getLocalizationKeyField()) {
    printInlinesUnder(LKF.getValue(), OS);
    return true;
  }

  return false;
}

bool ide::convertMarkupToXML(StringRef Text, raw_ostream &OS) {
  std::string Comment;
  {
    llvm::raw_string_ostream OS(Comment);
    OS << "/**\n" << Text << "\n" << "*/";
  }
  LineList LL = getLineListFromComment(Comment);
  MarkupContext MC;
  if (auto *Doc = swift::markup::parseDocument(MC, LL)) {
    CommentToXMLConverter Converter(OS);
    Converter.visitCommentParts(extractCommentParts(MC, Doc));
    OS.flush();
    return false;
  }
  return true;
}

//===----------------------------------------------------------------------===//
// Conversion to Doxygen.
//===----------------------------------------------------------------------===//

class DoxygenConverter : public MarkupASTVisitor<DoxygenConverter> {
  llvm::raw_ostream &OS;
  unsigned Indent;
  unsigned IsFreshLine : 1;
  unsigned IsEmptyComment : 1;

  void printIndent() {
    for (unsigned i = 0; i < Indent; ++i) {
      OS << ' ';
    }
  }

  void indent(unsigned Amount = 2) {
    Indent += Amount;
  }

  void dedent(unsigned Amount = 2) {
    Indent -= Amount;
  }

  void print(StringRef Str) {
    for (auto c : Str) {
      if (c == '\n') {
        if (IsFreshLine && !IsEmptyComment)
          OS << "///";
        IsFreshLine = true;
      } else {
        if (IsFreshLine && !IsEmptyComment)
          OS << "///";
        if (IsFreshLine) {
          printIndent();
          IsFreshLine = false;
        }
      }
      OS << c;
      IsEmptyComment = false;
    }
  }

  void printNestedParamField(const ParamField *PF) {
    auto Parts = PF->getParts().getValue();
    if (Parts.Brief.hasValue()) {
      visit(Parts.Brief.getValue());
    }

    if (!Parts.ParamFields.empty()) {
      printNewline();
      print("\\a ");
      print(PF->getName());
      print(" parameters:");
      printNewline();

      print("<ul>");
      printNewline();
      for (auto Param : Parts.ParamFields) {
        print("<li>");
        printNewline();
        print(Param->getName());
        print(": ");
        printNestedParamField(Param);
        print("</li>");
        printNewline();
      }
      print("</ul>");
      printNewline();
      printNewline();
    }

    if (Parts.ReturnsField.hasValue()) {
      printNewline();
      print("\\a ");
      print(PF->getName());
      print(" returns: ");

      for (auto Child : Parts.ReturnsField.getValue()->getChildren()) {
        visit(Child);
      }
    }

    if (Parts.ThrowsField.hasValue()) {
      printNewline();
      print("\\a ");
      print(PF->getName());
      print(" error: ");

      for (auto Child : Parts.ThrowsField.getValue()->getChildren()) {
        visit(Child);
      }
    }

    for (auto BodyNode : Parts.BodyNodes) {
      visit(BodyNode);
    }
  }

public:
  DoxygenConverter(llvm::raw_ostream &OS)
    : OS(OS), Indent(1), IsFreshLine(true), IsEmptyComment(true) {
    printOpeningComment();
  }

  void printNewline() {
    print("\n");
  }

  void printOpeningComment() {
    OS << "///";
  }

  void printUncommentedNewline() {
    OS << '\n';
  }

  void visitDocument(const Document *D) {
    for (const auto *Child : D->getChildren())
      visit(Child);
  }

  void visitBlockQuote(const BlockQuote *BQ) {
    print("<blockquote>");
    printNewline();
    for (const auto *Child : BQ->getChildren())
      visit(Child);
    printNewline();
    print("</blockquote>");
    printNewline();
  }

  void visitList(const List *BL) {
    print(BL->isOrdered() ? "<ol>" : "<ul>");
    indent();
    printNewline();
    for (const auto *Child : BL->getChildren())
      visit(Child);
    dedent();
    print(BL->isOrdered() ? "</ol>" : "</ul>");
    printNewline();
  }

  void visitItem(const Item *I) {
    print("<li>");
    indent();
    printNewline();
    for (const auto *N : I->getChildren())
      visit(N);
    dedent();
    print("</li>");
    printNewline();
  }

  void visitCodeBlock(const CodeBlock *CB) {
    print("\\code");
    printNewline();
    print(CB->getLiteralContent());
    printNewline();
    print("\\endcode");
  }

  void visitCode(const Code *C) {
    print("<code>");
    print(C->getLiteralContent());
    print("</code>");
  }

  void visitHTML(const HTML *H) {
    print(H->getLiteralContent());
  }

  void visitInlineHTML(const InlineHTML *IH) {
    print(IH->getLiteralContent());
  }

  void visitSoftBreak(const SoftBreak *SB) {
    printNewline();
  }

  void visitLineBreak(const LineBreak *LB) {
    print("<br/>");
    printNewline();
  }

  void visitLink(const Link *L) {
    SmallString<32> Tag;
    llvm::raw_svector_ostream S(Tag);
    S << "<a href=\"" << L->getDestination() << "\">";
    print(S.str());
    for (const auto *Child : L->getChildren())
      visit(Child);
    print("</a>");
  }

  void visitImage(const Image *I) {
    SmallString<64> Tag;
    llvm::raw_svector_ostream S(Tag);
    S << "<img src=\"" << I->getDestination() << "\"";
    if (I->hasTitle())
      S << " title=\"" << I->getTitle() << "\"";
    if (I->getChildren().size()) {
      S << " alt=\"";
      for (const auto *Child : I->getChildren())
        printInlinesUnder(Child, S);
      S << "\"";
    }
    S << "\\>";
    print(S.str());
  }

  void visitParagraph(const Paragraph *P) {
    for (const auto *Child : P->getChildren())
      visit(Child);
    printNewline();
  }

  void visitEmphasis(const Emphasis *E) {
    print("<em>");
    for (const auto *Child : E->getChildren())
      visit(Child);
    print("</em>");
  }

  void visitStrong(const Strong *E) {
    print("<em>");
    for (const auto *Child : E->getChildren())
      visit(Child);
    print("</em>");
  }

  void visitHRule(const HRule *HR) {
    print("<hr/>");
    printNewline();
  }

  void visitHeader(const Header *H) {
    llvm::SmallString<4> Tag;
    llvm::raw_svector_ostream TagStream(Tag);
    TagStream << "<h" << H->getLevel() << ">";
    print(TagStream.str());
    for (const auto *Child : H->getChildren())
      visit(Child);
    llvm::SmallString<5> EndTag;
    llvm::raw_svector_ostream EndTagStream(EndTag);
    EndTagStream << "</h" << H->getLevel() << ">";
    print(EndTagStream.str());
    printNewline();
  }

  void visitText(const Text *T) {
    print(T->getLiteralContent());
  }

  void visitPrivateExtension(const PrivateExtension *PE) {
    llvm_unreachable("Can't directly print Doxygen for a Swift Markup PrivateExtension");
  }

  void visitParamField(const ParamField *PF) {
    print("\\param ");
    print(PF->getName());
    print(" ");
    if (PF->isClosureParameter()) {
      printNestedParamField(PF);
    } else {
      for (const auto *Child : PF->getChildren()) {
        visit(Child);
      }
    }
    printNewline();
  }

  void visitReturnField(const ReturnsField *RF) {
    print("\\returns ");
    for (const auto *Child : RF->getChildren())
      visit(Child);
    printNewline();
  }

  void visitThrowField(const ThrowsField *TF) {
    print("\\param error ");
    for (const auto *Child : TF->getChildren())
      visit(Child);
    printNewline();
  }

#define MARKUP_SIMPLE_FIELD(Id, Keyword, XMLKind) \
  void visit##Id(const Id *Field) { \
    print(#Keyword); \
    print(":"); \
    printNewline(); \
    for (const auto *Child : Field->getChildren()) \
      visit(Child); \
  }
#include "swift/Markup/SimpleFields.def"

  ~DoxygenConverter() override {
    if (IsEmptyComment || !IsFreshLine)
      printUncommentedNewline();
  }
};

void ide::getDocumentationCommentAsDoxygen(const DocComment *DC,
                                           raw_ostream &OS) {
  DoxygenConverter Converter(OS);

  auto Brief = DC->getBrief();
  if (Brief.hasValue()) {
    Converter.visit(Brief.getValue());
  }

  for (const auto *N : DC->getBodyNodes()) {
    if (const auto *P = dyn_cast<Paragraph>(N)) {
      Converter.visit(P);
      continue;
    }
    Converter.visit(N);
  }

  for (const auto PF : DC->getParamFields()) {
    Converter.visit(PF);
  }

  auto TF = DC->getThrowsField();
  if (TF.hasValue()) {
    Converter.printNewline();
    Converter.visit(TF.getValue());
  }

  auto RF = DC->getReturnsField();
  if (RF.hasValue()) {
    Converter.printNewline();
    Converter.visit(RF.getValue());
  }
}

