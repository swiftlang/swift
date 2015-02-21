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
#include "swift/AST/CommentAST.h"
#include "swift/AST/Decl.h"
#include "swift/AST/USRGeneration.h"
#include "swift/Basic/SourceManager.h"
#include "swift/ReST/AST.h"
#include "swift/ReST/XMLUtils.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/Index/CommentToXML.h"

using namespace llvm::rest;
using namespace swift;

//===----------------------------------------------------------------------===//
// Conversion to XML.
//===----------------------------------------------------------------------===//

namespace {
struct CommentToXMLConverter {
  CommentContext &TheCommentContext;
  raw_ostream &OS;

  CommentToXMLConverter(CommentContext &TheCommentContext, raw_ostream &OS)
      : TheCommentContext(TheCommentContext), OS(OS) {}

  void printRawHTML(StringRef Tag) {
    OS << "<rawHTML>";
    appendWithCDATAEscaping(OS, Tag);
    OS << "</rawHTML>";
  }

  void printASTNode(const ReSTASTNode *N) {
    switch (N->getKind()) {
    case ASTNodeKind::Document:
      llvm_unreachable("should never happen");
      break;

    case ASTNodeKind::Section:
    case ASTNodeKind::Topic:
    case ASTNodeKind::Sidebar:
    case ASTNodeKind::Title:
    case ASTNodeKind::Subtitle:
    case ASTNodeKind::Transition:
      llvm_unreachable("implement");

    case ASTNodeKind::Paragraph:
      printParagraph(cast<Paragraph>(N));
      break;
    case ASTNodeKind::BulletList:
      printBulletList(cast<BulletList>(N));
      break;
    case ASTNodeKind::EnumeratedList:
      printEnumeratedList(cast<EnumeratedList>(N));
      break;
    case ASTNodeKind::DefinitionListItem:
      printDefinitionListItem(cast<DefinitionListItem>(N));
      break;
    case ASTNodeKind::DefinitionList:
      printDefinitionList(cast<DefinitionList>(N));
      break;
    case ASTNodeKind::Field:
      printField(cast<Field>(N));
      break;
    case ASTNodeKind::FieldList:
      printFieldList(cast<FieldList>(N));
      break;
    case ASTNodeKind::BlockQuote:
      printBlockQuote(cast<BlockQuote>(N));
      break;
    case ASTNodeKind::TextAndInline:
      printTextAndInline(cast<TextAndInline>(N));
      break;
    case ASTNodeKind::PlainText:
      printPlainText(cast<PlainText>(N));
      break;
    case ASTNodeKind::Emphasis:
      printEmphasis(cast<Emphasis>(N));
      break;
    case ASTNodeKind::StrongEmphasis:
      printStrongEmphasis(cast<StrongEmphasis>(N));
      break;
    case ASTNodeKind::InterpretedText:
      printInterpretedText(cast<InterpretedText>(N));
      break;
    case ASTNodeKind::InlineLiteral:
      printInlineLiteral(cast<InlineLiteral>(N));
      break;
    case ASTNodeKind::HyperlinkReference:
      printHyperlinkReference(cast<HyperlinkReference>(N));
      break;
    case ASTNodeKind::InlineHyperlinkTarget:
      printInlineHyperlinkTarget(cast<InlineHyperlinkTarget>(N));
      break;
    case ASTNodeKind::FootnoteReference:
      printFootnoteReference(cast<FootnoteReference>(N));
      break;
    case ASTNodeKind::CitationReference:
      printCitationReference(cast<CitationReference>(N));
      break;
    case ASTNodeKind::SubstitutionReference:
      printSubstitutionReference(cast<SubstitutionReference>(N));
      break;

    case ASTNodeKind::PrivateExtension:
      llvm_unreachable("implement");
    }
  }

  void printParagraph(const Paragraph *P) {
    printTextAndInline(P->getContent());
  }

  void printBulletList(const BulletList *BL) {
    printRawHTML("<ul>");
    for (unsigned i = 0, e = BL->getNumItems(); i != e; ++i) {
      printRawHTML("<li>");
      for (const auto *N : BL->getItemChildren(i)) {
        printASTNode(N);
      }
      printRawHTML("</li>");
    }
    printRawHTML("</ul>");
  }

  void printEnumeratedList(const EnumeratedList *EL) {
    printRawHTML("<ol>");
    for (unsigned i = 0, e = EL->getNumItems(); i != e; ++i) {
      printRawHTML("<li>");
      for (const auto *N : EL->getItemChildren(i)) {
        printASTNode(N);
      }
      printRawHTML("</li>");
    }
    printRawHTML("</ol>");
  }

  void printDefinitionListItem(const DefinitionListItem *DLI) {
    printRawHTML("<dt>");
    printASTNode(DLI->getTerm());
    printRawHTML("</dt>");
    for (const auto *N : DLI->getClassifiers()) {
      printASTNode(N);
    }

    printRawHTML("<dd>");
    for (const auto *N : DLI->getDefinitionChildren()) {
      printASTNode(N);
    }
    printRawHTML("</dd>");
  }

  void printDefinitionList(const DefinitionList *DL) {
    printRawHTML("<dl>");
    for (const auto *N : DL->getChildren()) {
      printASTNode(N);
    }
    printRawHTML("</dl>");
  }

  void printField(const Field *F) {
    printRawHTML("<dt>");
    printASTNode(F->getName());
    printRawHTML("</dt>");

    printRawHTML("<dd>");
    for (const auto *N : F->getBodyChildren()) {
      printASTNode(N);
    }
    printRawHTML("</dd>");
  }

  void printFieldList(const FieldList *FL) {
    printRawHTML("<dl>");
    for (const auto *F : FL->getChildren()) {
      printASTNode(F);
    }
    printRawHTML("</dl>");
  }

  void printBlockQuote(const BlockQuote *BQ) {
    bool HacksEnabled =
        TheCommentContext.TheReSTContext.LangOpts.TemporaryHacks;

    if (HacksEnabled)
      OS << "<Verbatim kind=\"code\" xml:space=\"preserve\">";
    else
      printRawHTML("<blockquote>");

    for (const auto *N : BQ->getChildren()) {
      printASTNode(N);
    }

    if (HacksEnabled)
      OS << "</Verbatim>";
    else
      printRawHTML("</blockquote>");
  }

  void printTextAndInline(const TextAndInline *T) {
    for (const auto *IC : T->getChildren()) {
      printASTNode(IC);
    }
  }

  void printPlainText(const PlainText *PT) {
    bool HacksEnabled =
        TheCommentContext.TheReSTContext.LangOpts.TemporaryHacks;

    if (HacksEnabled) {
      for (auto Parts = std::make_pair(StringRef(), PT->getLinePart().Text);
           !Parts.first.empty() || !Parts.second.empty();
           Parts = Parts.second.split("\\ ")) {
        appendWithXMLEscaping(OS, Parts.first);
      }
    } else {
      appendWithXMLEscaping(OS, PT->getLinePart().Text);
    }
  }

  void printEmphasis(const Emphasis *E) {
    printRawHTML("<em>");
    for (const auto *IC : E->getChildren()) {
      printASTNode(IC);
    }
    printRawHTML("</em>");
  }

  void printStrongEmphasis(const StrongEmphasis *SE) {
    printRawHTML("<strong>");
    for (const auto *IC : SE->getChildren()) {
      printASTNode(IC);
    }
    printRawHTML("</strong>");
  }

  void printInterpretedText(const InterpretedText *IT) {
    // FIXME: check role.
    printRawHTML("<code>");
    for (const auto *IC : IT->getChildren()) {
      printASTNode(IC);
    }
    printRawHTML("</code>");
  }

  void printInlineLiteral(const InlineLiteral *IL) {
    printRawHTML("<code>");
    for (const auto *IC : IL->getChildren()) {
      printASTNode(IC);
    }
    printRawHTML("</code>");
  }

  void printHyperlinkReference(const HyperlinkReference *HR) {
    // FIXME: print as a hyperlink.
    for (const auto *IC : HR->getChildren()) {
      printASTNode(IC);
    }
  }

  void printInlineHyperlinkTarget(const InlineHyperlinkTarget *IHT) {
    // FIXME: print link anchor.
    for (const auto *IC : IHT->getChildren()) {
      printASTNode(IC);
    }
  }

  void printFootnoteReference(const FootnoteReference *FR) {
    // FIXME: XML format does not support footnotes.  Skip them for now.
  }

  void printCitationReference(const CitationReference *CR) {
    // FIXME: XML format does not support citations.  Skip them for now.
  }

  void printSubstitutionReference(const SubstitutionReference *SR) {
    // FIXME: we don't resolve substitutions yet.
  }

  void printOrphanField(const Field *F) {
    printRawHTML("<dl>");
    printField(F);
    printRawHTML("</dl>");
  }

  void printAsParameter(const comments::ParamField *PF) {
    OS << "<Parameter><Name>";
    OS << PF->getParamName().Text;
    OS << "</Name><Direction isExplicit=\"0\">in</Direction><Discussion>";
    for (const auto *N : PF->getBodyChildren()) {
      OS << "<Para>";
      printASTNode(N);
      OS << "</Para>";
    }
    OS << "</Discussion></Parameter>";
  }

  void printAsReturns(const Field *F) {
    for (const auto *N : F->getBodyChildren()) {
      printASTNode(N);
    }
  }

  void visitFullComment(const FullComment *FC);
};
} // unnamed namespace

void CommentToXMLConverter::visitFullComment(const FullComment *FC) {
  const Decl *D = FC->getDecl();
  const auto &Parts = FC->getParts(TheCommentContext);

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
  if (VD && VD->hasName())
    OS << VD->getFullName();
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
    PrintOptions PO;
    PO.PrintDefaultParameterPlaceholder = true;
    PO.SkipImplicit = true;
    PO.PrintImplicitAttrs = false;
    PO.PrintFunctionRepresentationAttrs = false;
    OS << "<Declaration>";
    llvm::SmallString<32> DeclSS;
    {
      llvm::raw_svector_ostream DeclOS(DeclSS);
      D->print(DeclOS, PO);
    }
    appendWithXMLEscaping(OS, DeclSS);
    OS << "</Declaration>";
  }

  if (Parts.Brief) {
    OS << "<Abstract>";
    OS << "<Para>";
    printASTNode(Parts.Brief);
    OS << "</Para>";
    OS << "</Abstract>";
  }

  if (!Parts.MiscTopLevelNodes.empty()) {
    OS << "<Discussion>";
    for (const auto *N : Parts.MiscTopLevelNodes) {
      bool Verbatim =
         TheCommentContext.TheReSTContext.LangOpts.TemporaryHacks &&
         isa<BlockQuote>(N);

      if (!Verbatim)
        OS << "<Para>";

      if (const auto *F = dyn_cast<Field>(N)) {
        printOrphanField(F);
        OS << "</Para>";
        continue;
      }
      printASTNode(N);
      if (!Verbatim)
        OS << "</Para>";
    }
    OS << "</Discussion>";
  }

  if (!Parts.Params.empty()) {
    OS << "<Parameters>";
    for (const auto *N : Parts.Params) {
      printAsParameter(N);
    }
    OS << "</Parameters>";
  }
  if (!Parts.Returns.empty()) {
    OS << "<ResultDiscussion>";
    for (const auto *N : Parts.Returns) {
      OS << "<Para>";
      printAsReturns(N);
      OS << "</Para>";
    }
    OS << "</ResultDiscussion>";
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

  CommentContext TheCommentContext;
  TheCommentContext.TheReSTContext.LangOpts.TemporaryHacks =
      D->getASTContext().LangOpts.ReSTTemporaryHacks;

  auto *FC = getFullComment(TheCommentContext, D);
  if (!FC)
    return false;

  CommentToXMLConverter Converter(TheCommentContext, OS);
  Converter.visitFullComment(FC);

  OS.flush();
  return true;
}

//===----------------------------------------------------------------------===//
// Conversion to Doxygen.
//===----------------------------------------------------------------------===//

namespace {
struct CommentToDoxygenConverter {
  CommentContext &TheCommentContext;
  raw_ostream &OS;
  unsigned PendingNewlines = 1;

  CommentToDoxygenConverter(CommentContext &TheCommentContext, raw_ostream &OS)
      : TheCommentContext(TheCommentContext), OS(OS) {}

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

  void printASTNode(const ReSTASTNode *N) {
    switch (N->getKind()) {
    case ASTNodeKind::Document:
      llvm_unreachable("should never happen");
      break;

    case ASTNodeKind::Section:
    case ASTNodeKind::Topic:
    case ASTNodeKind::Sidebar:
    case ASTNodeKind::Title:
    case ASTNodeKind::Subtitle:
    case ASTNodeKind::Transition:
      llvm_unreachable("implement");

    case ASTNodeKind::Paragraph:
      printParagraph(cast<Paragraph>(N));
      break;
    case ASTNodeKind::BulletList:
      printBulletList(cast<BulletList>(N));
      break;
    case ASTNodeKind::EnumeratedList:
      printEnumeratedList(cast<EnumeratedList>(N));
      break;
    case ASTNodeKind::DefinitionListItem:
      printDefinitionListItem(cast<DefinitionListItem>(N));
      break;
    case ASTNodeKind::DefinitionList:
      printDefinitionList(cast<DefinitionList>(N));
      break;
    case ASTNodeKind::Field:
      printField(cast<Field>(N));
      break;
    case ASTNodeKind::FieldList:
      printFieldList(cast<FieldList>(N));
      break;
    case ASTNodeKind::BlockQuote:
      printBlockQuote(cast<BlockQuote>(N));
      break;
    case ASTNodeKind::TextAndInline:
      printTextAndInline(cast<TextAndInline>(N));
      break;
    case ASTNodeKind::PlainText:
      printPlainText(cast<PlainText>(N));
      break;
    case ASTNodeKind::Emphasis:
      printEmphasis(cast<Emphasis>(N));
      break;
    case ASTNodeKind::StrongEmphasis:
      printStrongEmphasis(cast<StrongEmphasis>(N));
      break;
    case ASTNodeKind::InterpretedText:
      printInterpretedText(cast<InterpretedText>(N));
      break;
    case ASTNodeKind::InlineLiteral:
      printInlineLiteral(cast<InlineLiteral>(N));
      break;
    case ASTNodeKind::HyperlinkReference:
      printHyperlinkReference(cast<HyperlinkReference>(N));
      break;
    case ASTNodeKind::InlineHyperlinkTarget:
      printInlineHyperlinkTarget(cast<InlineHyperlinkTarget>(N));
      break;
    case ASTNodeKind::FootnoteReference:
      printFootnoteReference(cast<FootnoteReference>(N));
      break;
    case ASTNodeKind::CitationReference:
      printCitationReference(cast<CitationReference>(N));
      break;
    case ASTNodeKind::SubstitutionReference:
      printSubstitutionReference(cast<SubstitutionReference>(N));
      break;

    case ASTNodeKind::PrivateExtension:
      llvm_unreachable("implement");
    }
  }

  void printParagraph(const Paragraph *P) {
    print("<p>");
    printTextAndInline(P->getContent());
    print("</p>");
  }

  void printBulletList(const BulletList *BL) {
    print("<ul>");
    for (unsigned i = 0, e = BL->getNumItems(); i != e; ++i) {
      print("<li>");
      for (const auto *N : BL->getItemChildren(i)) {
        printASTNode(N);
      }
      print("</li>");
    }
    print("</ul>");
  }

  void printEnumeratedList(const EnumeratedList *EL) {
    print("<ol>");
    for (unsigned i = 0, e = EL->getNumItems(); i != e; ++i) {
      print("<li>");
      for (const auto *N : EL->getItemChildren(i)) {
        printASTNode(N);
      }
      print("</li>");
    }
    print("</ol>");
  }

  void printDefinitionListItem(const DefinitionListItem *DLI) {
    print("<dt>");
    printASTNode(DLI->getTerm());
    for (const auto *N : DLI->getClassifiers()) {
      printASTNode(N);
    }
    print("</dt>");

    print("<dd>");
    for (const auto *N : DLI->getDefinitionChildren()) {
      printASTNode(N);
    }
    print("</dd>");
  }

  void printDefinitionList(const DefinitionList *DL) {
    print("<dl>");
    for (const auto *N : DL->getChildren()) {
      printASTNode(N);
    }
    print("</dl>");
  }

  void printField(const Field *F) {
    print("<dt>");
    printASTNode(F->getName());
    print("</dt>");

    print("<dd>");
    for (const auto *N : F->getBodyChildren()) {
      printASTNode(N);
    }
    print("</dd>");
  }

  void printFieldList(const FieldList *FL) {
    print("<dl>");
    for (const auto *F : FL->getChildren()) {
      printASTNode(F);
    }
    print("</dl>");
  }

  void printBlockQuote(const BlockQuote *BQ) {
    bool HacksEnabled =
        TheCommentContext.TheReSTContext.LangOpts.TemporaryHacks;

    print(HacksEnabled ? "<pre>" : "<blockquote>");

    for (const auto *N : BQ->getChildren()) {
      printASTNode(N);
    }

    print(HacksEnabled ? "</pre>" : "</blockquote>");
  }

  void printTextAndInline(const TextAndInline *T) {
    for (const auto *IC : T->getChildren()) {
      printASTNode(IC);
    }
  }

  void printPlainText(const PlainText *PT) {
    if (PT->getLinePart().Text == "\n")
      printNewline();
    else {
      bool HacksEnabled =
          TheCommentContext.TheReSTContext.LangOpts.TemporaryHacks;

      if (HacksEnabled) {
        for (auto Parts = std::make_pair(StringRef(), PT->getLinePart().Text);
             !Parts.first.empty() || !Parts.second.empty();
             Parts = Parts.second.split("\\ ")) {
          print(Parts.first);
        }
      } else {
        print(PT->getLinePart().Text);
      }
    }
  }

  void printEmphasis(const Emphasis *E) {
    print("<em>");
    for (const auto *IC : E->getChildren()) {
      printASTNode(IC);
    }
    print("</em>");
  }

  void printStrongEmphasis(const StrongEmphasis *SE) {
    print("<strong>");
    for (const auto *IC : SE->getChildren()) {
      printASTNode(IC);
    }
    print("</strong>");
  }

  void printInterpretedText(const InterpretedText *IT) {
    // FIXME: check role.
    print("<code>");
    for (const auto *IC : IT->getChildren()) {
      printASTNode(IC);
    }
    print("</code>");
  }

  void printInlineLiteral(const InlineLiteral *IL) {
    print("<code>");
    for (const auto *IC : IL->getChildren()) {
      printASTNode(IC);
    }
    print("</code>");
  }

  void printHyperlinkReference(const HyperlinkReference *HR) {
    // FIXME: print as a hyperlink.
    for (const auto *IC : HR->getChildren()) {
      printASTNode(IC);
    }
  }

  void printInlineHyperlinkTarget(const InlineHyperlinkTarget *IHT) {
    // FIXME: print link anchor.
    for (const auto *IC : IHT->getChildren()) {
      printASTNode(IC);
    }
  }

  void printFootnoteReference(const FootnoteReference *FR) {
    // Doxygen does not support footnotes.
  }

  void printCitationReference(const CitationReference *CR) {
    // Doxygen does not support citations.
  }

  void printSubstitutionReference(const SubstitutionReference *SR) {
    // FIXME: we don't resolve substitutions yet.
  }

  void printOrphanField(const Field *F) {
    print("<dl>");
    printField(F);
    print("</dl>");
  }

  void printBlockCommandContent(ArrayRef<const ReSTASTNode *> Nodes) {
    if (Nodes.size() == 0)
      return;
    print(" ");
    if (Nodes.size() == 1) {
      if (const auto *P = dyn_cast<Paragraph>(Nodes[0])) {
        printTextAndInline(P->getContent());
        return;
      }
    }
    for (const auto *N : Nodes) {
      printASTNode(N);
    }
  }

  void printAsDoxygenParam(const comments::ParamField *PF) {
    print("\\param ");
    print(PF->getParamName().Text);
    printBlockCommandContent(PF->getBodyChildren());
    printNewline();
  }

  void printAsDoxygenReturns(const Field *F) {
    print("\\returns");
    printBlockCommandContent(F->getBodyChildren());
    printNewline();
  }
};
} // unnamed namespace

void ide::getDocumentationCommentAsDoxygen(CommentContext &TheCommentContext,
                                           const FullComment *FC,
                                           raw_ostream &OS) {
  CommentToDoxygenConverter Converter(TheCommentContext, OS);
  const auto &Parts = FC->getParts(TheCommentContext);

  if (Parts.Brief) {
    Converter.printTextAndInline(Parts.Brief->getContent());
    Converter.printNewline();
    Converter.printNewline();
  }

  for (const auto *N : Parts.MiscTopLevelNodes) {
    if (const auto *F = dyn_cast<Field>(N)) {
      Converter.printOrphanField(F);
      Converter.printNewline();
      continue;
    }
    if (const auto *P = dyn_cast<Paragraph>(N)) {
      Converter.printTextAndInline(P->getContent());
      Converter.printNewline();
      Converter.printNewline();
      continue;
    }
    Converter.printASTNode(N);
    Converter.printNewline();
  }
  for (const auto *N : Parts.Params) {
    Converter.printAsDoxygenParam(N);
    Converter.printNewline();
  }
  for (const auto *N : Parts.Returns) {
    Converter.printAsDoxygenReturns(N);
    Converter.printNewline();
  }
  if (Converter.PendingNewlines != 0)
    OS << "\n";
}
