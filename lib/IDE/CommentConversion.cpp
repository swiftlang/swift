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
  raw_ostream &OS;

  CommentToXMLConverter(raw_ostream &OS) : OS(OS) {}

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
    }
  }

  void printParagraph(const Paragraph *P) {
    OS << "<Para>";
    printTextAndInline(P->getContent());
    OS << "</Para>";
  }

  void printBulletList(const BulletList *BL) {
    // FIXME: print block markup.
    for (unsigned i = 0, e = BL->getNumItems(); i != e; ++i) {
      for (const auto *N : BL->getItemChildren(i)) {
        printASTNode(N);
      }
    }
  }

  void printEnumeratedList(const EnumeratedList *EL) {
    // FIXME: print block markup.
    for (unsigned i = 0, e = EL->getNumItems(); i != e; ++i) {
      for (const auto *N : EL->getItemChildren(i)) {
        printASTNode(N);
      }
    }
  }

  void printDefinitionListItem(const DefinitionListItem *DLI) {
    // FIXME: print real block markup.
    OS << "<Para>";
    printASTNode(DLI->getTerm());
    OS << "</Para>";
    for (const auto *N : DLI->getClassifiers()) {
      printASTNode(N);
    }

    for (const auto *N : DLI->getDefinitionChildren()) {
      printASTNode(N);
    }
  }

  void printDefinitionList(const DefinitionList *DL) {
    // FIXME: print block markup.
    for (const auto *N : DL->getChildren()) {
      printASTNode(N);
    }
  }

  void printField(const Field *F) {
    // FIXME: print real block markup.
    OS << "<Para>";
    printASTNode(F->getName());
    OS << "</Para>";
    for (const auto *N : F->getBodyChildren()) {
      printASTNode(N);
    }
  }

  void printFieldList(const FieldList *FL) {
    // FIXME: print block markup.
    for (const auto *F : FL->getChildren()) {
      printASTNode(F);
    }
  }

  void printBlockQuote(const BlockQuote *BQ) {
    // FIXME: print block markup.
    for (const auto *N : BQ->getChildren()) {
      printASTNode(N);
    }
  }

  void printTextAndInline(const TextAndInline *T) {
    if (T->isLinePart()) {
      LinePart LP = T->getLinePart();
      appendWithXMLEscaping(OS, LP.Text);
    } else {
      LineListRef LL = T->getLines();
      for (unsigned i = 0, e = LL.size(); i != e; ++i) {
        appendWithXMLEscaping(OS, LL[i].Text.drop_front(LL[i].FirstTextByte));
        if (i != e - 1)
          OS << " ";
      }
    }
  }

  void printOrphanField(const Field *F) {
    // FIXME: print block markup.
    printField(F);
  }

  void printAsParameter(const Field *F) {
    OS << "<Parameter><Name>";
    // FIXME: extract parameter name.
    OS << "x";
    OS << "</Name><Direction isExplicit=\"0\">in</Direction><Discussion>";
    for (const auto *N : F->getBodyChildren()) {
      printASTNode(N);
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
  const auto &Parts = FC->getParts();

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
      StringRef FileName = SM->getMemoryBuffer(BufferID)->getBufferIdentifier();
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

  // FIXME: <Declaration>
  if (Parts.Brief) {
    OS << "<Abstract>";
    printASTNode(Parts.Brief);
    OS << "</Abstract>";
  }

  if (!Parts.MiscTopLevelNodes.empty()) {
    OS << "<Discussion>";
    for (const auto *N : Parts.MiscTopLevelNodes) {
      if (const auto *F = dyn_cast<Field>(N)) {
        printOrphanField(F);
        continue;
      }
      printASTNode(N);
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
      printAsReturns(N);
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
  auto *FC = getFullComment(TheCommentContext, D);
  if (!FC)
    return false;

  CommentToXMLConverter Converter(OS);
  Converter.visitFullComment(FC);

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
    print("<blockquote>");
    for (const auto *N : BQ->getChildren()) {
      printASTNode(N);
    }
    print("</blockquote>");
  }

  void printTextAndInline(const TextAndInline *T) {
    if (T->isLinePart()) {
      LinePart LP = T->getLinePart();
      print(LP.Text);
    } else {
      LineListRef LL = T->getLines();
      for (unsigned i = 0, e = LL.size(); i != e; ++i) {
        print(LL[i].Text.drop_front(LL[i].FirstTextByte));
        if (i != e - 1)
          printNewline();
      }
    }
  }

  void printOrphanField(const Field *F) {
    print("<dl>");
    printField(F);
    print("</dl>");
  }

  void printBlockCommandContent(ArrayRef<const ReSTASTNode *> Nodes) {
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

  void printAsDoxygenParam(const Field *F) {
    print("\\param ");
    printBlockCommandContent(F->getBodyChildren());
    printNewline();
  }

  void printAsDoxygenReturns(const Field *F) {
    print("\\returns ");
    printBlockCommandContent(F->getBodyChildren());
    printNewline();
  }
};
} // unnamed namespace

void ide::getDocumentationCommentAsDoxygen(const FullComment *FC, raw_ostream &OS) {
  CommentToDoxygenConverter Converter(OS);
  const auto &Parts = FC->getParts();

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

