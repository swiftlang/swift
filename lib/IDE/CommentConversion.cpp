//===--- CommentConversion.cpp - Conversion of comments to other formats --===//
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

#include "swift/IDE/CommentConversion.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Comment.h"
#include "swift/AST/Decl.h"
#include "swift/AST/USRGeneration.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Markup/Markup.h"
#include "swift/Markup/XMLUtils.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/Index/CommentToXML.h"

using namespace llvm::markup;
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
    llvm_unreachable("Can't print an llvm::markup::Document as XML directly");
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
    OS << "<CodeListing>";
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
    S << "<Link href=\"" << L->getDestination() << "\">";
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
    S << "<img src=\"" << I->getDestination() << "\"/>";
    printRawHTML(S.str());
  }

  void printParamField(const ParamField *PF) {
    OS << "<Parameter><Name>";
    OS << PF->getName();
    OS << "</Name><Direction isExplicit=\"0\">in</Direction><Discussion>";
    for (auto Child : PF->getChildren())
      printASTNode(Child);

    OS << "</Discussion></Parameter>";
  }

  void printResultDiscussion(const ReturnsField *RF) {
    OS << "<ResultDiscussion>";
    for (auto Child : RF->getChildren())
      printASTNode(Child);
    OS << "</ResultDiscussion>";
  }

  void visitDocComment(const DocComment *DC);
};
} // unnamed namespace

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

  auto Brief = DC->getBrief();
  if (Brief.hasValue()) {
    OS << "<Abstract>";
    printASTNode(Brief.getValue());
    OS << "</Abstract>";
  }

  if (!DC->getParamFields().empty()) {
    OS << "<Parameters>";
    for (const auto *PF : DC->getParamFields())
      printParamField(PF);

    OS << "</Parameters>";
  }

  auto RF = DC->getReturnsField();
  if (RF.hasValue())
    printResultDiscussion(RF.getValue());

  if (!DC->getBodyNodes().empty()) {
    OS << "<Discussion>";
    for (const auto *N : DC->getBodyNodes())
      printASTNode(N);

    OS << "</Discussion>";
  }

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

bool ide::getDocumentationCommentAsXML(const Decl *D, raw_ostream &OS) {
  auto MaybeClangNode = D->getClangNode();
  if (MaybeClangNode) {
    if (auto *CD = MaybeClangNode.getAsDecl())
      return getClangDocumentationCommentAsXML(CD, OS);
    return false;
  }

  llvm::markup::MarkupContext MC;
  auto DC = getDocComment(MC, D);
  if (!DC.hasValue())
    return false;

  CommentToXMLConverter Converter(OS);
  Converter.visitDocComment(DC.getValue());

  OS.flush();
  return true;
}

//===----------------------------------------------------------------------===//
// Conversion to Doxygen.
//===----------------------------------------------------------------------===//

namespace {
struct CommentToDoxygenConverter {
  raw_ostream &OS;
  unsigned PendingNewlines = 1;

  CommentToDoxygenConverter(raw_ostream &OS) : OS(OS) {}

  void print(StringRef Text) {
    for (unsigned i = 0; i != PendingNewlines; ++i) {
      OS << "\n///";
      if (i == PendingNewlines - 1)
        OS << " ";
    }
    PendingNewlines = 0;
    OS << Text;
  }

  void printNewline() {
    PendingNewlines++;
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
    OS << "\\" << #XMLKind << " "; \
    for (auto Child : Field->getChildren()) \
      printASTNode(Child); \
      printNewline(); \
  }
#include "swift/Markup/SimpleFields.def"

  void printDocument(const Document *D) {
    // FIXME: Why keep doing this?
    llvm_unreachable("Can't print an llvm::markup::Document as XML directly");
  }

  void printBlockQuote(const BlockQuote *BQ) {
    print("<blockquote>");
    for (const auto *N : BQ->getChildren())
      printASTNode(N);

    print("</blockquote>");
  }

  void printList(const List *BL) {
    print(BL->isOrdered() ? "<ol>" : "<ul>");
    for (const auto *I : BL->getChildren())
      printASTNode(I);

    print(BL->isOrdered() ? "</ol>" : "</ul>");
  }

  void printItem(const Item *I) {
    print("<li>");
    for (const auto *N : I->getChildren())
      printASTNode(N);

    print("</li>");
  }

  void printCodeBlock(const CodeBlock *CB) {
    print("<code>");
    SmallVector<StringRef, 8> Lines;
    CB->getLiteralContent().split(Lines, "\n");
    for (auto Line : Lines) {
      print(Line);
      printNewline();
    }
    print("</code>");
  }

  void printCode(const Code *C) {
    print("<code>");
    SmallVector<StringRef, 8> Lines;
    C->getLiteralContent().split(Lines, "\n");
    for (auto Line : Lines) {
      print(Line);
      printNewline();
    }
    print("</code>");
  }

  void printHTML(const HTML *H) {
    print(H->getLiteralContent().str());
  }

  void printInlineHTML(const InlineHTML *IH) {
    print(IH->getLiteralContent().str());
  }

  void printSoftBreak(const SoftBreak *SB) {
    printNewline();
  }

  void printLineBreak(const LineBreak *LB) {
    print("<br/>");
    printNewline();
  }

  void printLink(const Link *L) {
    SmallString<32> Tag;
    llvm::raw_svector_ostream S(Tag);
    S << "<a href=\"" << L->getDestination() << "\">";
    print(S.str());

    for (const auto N : L->getChildren())
      printASTNode(N);

    print("</a>");
  }

  void printImage(const Image *I) {
    SmallString<64> Tag;
    llvm::raw_svector_ostream S(Tag);
    S << "<img src=\"" << I->getDestination() << "\"/>";
    print(S.str());
  }

  void printParagraph(const Paragraph *P) {
    for (const auto *N : P->getChildren())
      printASTNode(N);
  }

  void printEmphasis(const Emphasis *E) {
    print("<em>");
    for (const auto *N : E->getChildren())
      printASTNode(N);

    print("</em>");
  }

  void printStrong(const Strong *E) {
    print("<em>");
    for (const auto *N : E->getChildren())
      printASTNode(N);

    print("</em>");
  }

  void printHRule(const HRule *HR) {
    print("<hr/>");
  }

  void printHeader(const Header *H) {
    llvm::SmallString<4> Tag;
    llvm::raw_svector_ostream TagStream(Tag);
    TagStream << "<h" << H->getLevel() << ">";
    print(TagStream.str());
    for (auto Child : H->getChildren())
      printASTNode(Child);

    llvm::SmallString<5> EndTag;
    llvm::raw_svector_ostream EndTagStream(EndTag);
    EndTagStream << "</h" << H->getLevel() << ">";
    print(EndTagStream.str());
  }

  void printText(const Text *T) {
    print(T->getLiteralContent().str());
  }

  void printPrivateExtension(const PrivateExtension *PE) {
    llvm_unreachable("Can't directly print Doxygen for a Swift Markup PrivateExtension");
  }

  void printParamField(const ParamField *PF) {
    print("\\param ");
    print(PF->getName());
    print(" ");
    for (auto Child : PF->getChildren())
      printASTNode(Child);

    printNewline();
  }

  void printReturnField(const ReturnsField *RF) {
    print("\\returns ");
    print(" ");
    for (auto Child : RF->getChildren())
      printASTNode(Child);

    printNewline();
  }
};
} // unnamed namespace

void ide::getDocumentationCommentAsDoxygen(const DocComment *DC,
                                           raw_ostream &OS) {
  CommentToDoxygenConverter Converter(OS);

  auto Brief = DC->getBrief();
  if (Brief.hasValue()) {
    SmallString<256> BriefStr;
    llvm::raw_svector_ostream OS(BriefStr);
    llvm::markup::printInlinesUnder(Brief.getValue(), OS);
    Converter.print(OS.str());
    Converter.printNewline();
    Converter.printNewline();
  }

  for (const auto *N : DC->getBodyNodes()) {
    if (const auto *P = dyn_cast<Paragraph>(N)) {
      Converter.printASTNode(P);
      Converter.printNewline();
      Converter.printNewline();
      continue;
    }
    Converter.printASTNode(N);
    Converter.printNewline();
  }

  for (const auto PF : DC->getParamFields()) {
    Converter.printParamField(PF);
    Converter.printNewline();
  }

  auto RF = DC->getReturnsField();
  if (RF.hasValue()) {
    Converter.printReturnField(RF.getValue());
    Converter.printNewline();
  }

  if (Converter.PendingNewlines != 0)
    OS << "\n";
}
