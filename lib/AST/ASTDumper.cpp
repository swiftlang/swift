//===--- ASTDumper.cpp - Swift Language AST Dumper-------------------------===//
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
//  This file implements dumping for the Swift ASTs.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/AST.h"
#include "swift/AST/ASTVisitor.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/Optional.h"
#include "llvm/Support/Process.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

#define DEF_COLOR(NAME, COLOR)\
static const llvm::raw_ostream::Colors NAME##Color = llvm::raw_ostream::COLOR;

DEF_COLOR(Func, YELLOW)
DEF_COLOR(Extension, MAGENTA)
DEF_COLOR(SourceRange, CYAN)

#undef DEF_COLOR

//===----------------------------------------------------------------------===//
//  Decl printing.
//===----------------------------------------------------------------------===//

namespace {
  class PrintPattern : public PatternVisitor<PrintPattern> {
  public:
    raw_ostream &OS;
    PrintPattern(raw_ostream &os) : OS(os) {}

    void visitParenPattern(ParenPattern *P) {
      OS << '(';
      visit(P->getSubPattern());
      OS << ')';
    }
    void visitTuplePattern(TuplePattern *P) {
      OS << '(';
      for (unsigned i = 0, e = P->getNumFields(); i != e; ++i) {
        visit(P->getFields()[i].getPattern());
        if (i + 1 != e)
          OS << ", ";
      }
      OS << ')';
    }
    void visitNamedPattern(NamedPattern *P) {
      OS << P->getBoundName().str();
    }
    void visitAnyPattern(AnyPattern *P) {
      OS << '_';
    }
    void visitTypedPattern(TypedPattern *P) {
      visit(P->getSubPattern());
      OS << " : ";
      if (!P->hasType())
        OS << "<no type yet>";
      else
        P->getType()->print(OS);
    }
    
    void visitIsaPattern(IsaPattern *P) {
      OS << "is ";
      P->getCastTypeLoc().getType()->print(OS);
    }
    void visitNominalTypePattern(NominalTypePattern *P) {
      P->getCastTypeLoc().getType()->print(OS);
      P->getSubPattern()->print(OS);
    }
    void visitExprPattern(ExprPattern *P) {
      P->getSubExpr()->print(OS);
    }
    void visitVarPattern(VarPattern *P) {
      OS << "var ";
      P->getSubPattern()->print(OS);
    }
  };

  /// PrintDecl - Visitor implementation of Decl::print.
  class PrintDecl : public DeclVisitor<PrintDecl> {
  public:
    raw_ostream &OS;
    unsigned Indent;
    bool ShowColors;
    int &LastBuffer;
    int &LastLine;
    const llvm::SourceMgr &SourceMgr;

    PrintDecl(raw_ostream &os, unsigned indent, int &lastBuffer, int &lastLine,
              const llvm::SourceMgr &SM)
      : OS(os), Indent(indent), ShowColors(false),
        LastBuffer(lastBuffer), LastLine(lastLine), SourceMgr(SM) {
      if (&os == &llvm::errs() || &os == &llvm::outs())
    ShowColors = llvm::errs().has_colors() && llvm::outs().has_colors();
    }
    
    void printRec(Decl *D) {
      PrintDecl(OS, Indent+2, LastBuffer, LastLine, SourceMgr).visit(D);
    }
    void printRec(Expr *E, bool NoIndent = false);
    void printRec(Stmt *S);

    void printGenericParameters(GenericParamList *Params) {
      if (!Params)
        return;

      OS << '<';
      bool First = true;
      for (auto P : *Params) {
        if (First) {
          First = false;
        } else {
          OS << ", ";
        }
        OS << P.getDecl()->getName();
        if (!P.getAsTypeParam()->getInherited().empty()) {
          OS << " : ";
          P.getAsTypeParam()->getInherited()[0].getType()->print(OS);
        }
      }
      OS << '>';
    }

    void printCommon(Decl *D, const char *Name,
                     llvm::Optional<llvm::raw_ostream::Colors> Color =
                      llvm::Optional<llvm::raw_ostream::Colors>()) {
      OS.indent(Indent) << '(';

      // Support optional color output.
      if (ShowColors && Color.hasValue()) {
        if (const char *CStr =
            llvm::sys::Process::OutputColor(Color.getValue(), false, false)) {
          OS << CStr;
        }
      }

      OS << Name;

      if (ShowColors)
        OS << llvm::sys::Process::ResetColor();

      if (auto value = dyn_cast<ValueDecl>(D)) {
        if (!value->getName().empty()) {
          OS << " \"" << value->getName().str() << "\"";
        }
      }

      OS << ' ';
      if (ShowColors) {
        if (const char *CStr =
            llvm::sys::Process::OutputColor(SourceRangeColor, false, false)) {
          OS << CStr;
        }
      }
      D->getSourceRange().print(OS, SourceMgr, LastBuffer, LastLine,
                                /*PrintText=*/false);
      if (ShowColors)
        OS << llvm::sys::Process::ResetColor();
    }

    void printInherited(ArrayRef<TypeLoc> Inherited) {
      if (Inherited.empty())
        return;
      OS << " inherits: ";
      bool First = true;
      for (auto Super : Inherited) {
        if (First)
          First = false;
        else
          OS << ", ";

        Super.getType()->print(OS);
      }
    }

    void visitImportDecl(ImportDecl *ID) {
      printCommon(ID, "import_decl");
      OS << " '" << ID->getAccessPath()[0].first;
      for (unsigned i = 1, e = ID->getAccessPath().size(); i != e; ++i)
        OS << "." << ID->getAccessPath()[i].first;
      OS << "')";
    }

    void visitExtensionDecl(ExtensionDecl *ED) {
      printCommon(ED, "extension_decl", ExtensionColor);
      OS << ' ';
      ED->getExtendedType()->print(OS);
      printInherited(ED->getInherited());
      for (Decl *Member : ED->getMembers()) {
        if (Member->isImplicit())
          continue;

        OS << '\n';
        printRec(Member);
      }
      OS << ")";
    }

    void printDeclName(ValueDecl *D) {
      if (D->getName().get())
        OS << '\'' << D->getName() << '\'';
      else
        OS << "'anonname=" << (const void*)D << '\'';
    }

    void visitTypeAliasDecl(TypeAliasDecl *TAD) {
      printCommon(TAD, "typealias");
      OS << " type='";
      if (TAD->hasUnderlyingType())
        TAD->getUnderlyingType()->print(OS);
      else
        OS << "<<<unresolved>>>";
      printInherited(TAD->getInherited());
      OS << "')";
    }

    void visitProtocolDecl(ProtocolDecl *PD) {
      printCommon(PD, "protocol");
      printInherited(PD->getInherited());
      for (auto VD : PD->getMembers()) {
        if (VD->isImplicit())
          continue;

        OS << '\n';
        printRec(VD);
      }
      OS << ")";
    }

    void printCommon(ValueDecl *VD, const char *Name) {
      printCommon((Decl*)VD, Name);
      OS << ' ';
      printDeclName(VD);
      if (FuncDecl *FD = dyn_cast<FuncDecl>(VD))
        printGenericParameters(FD->getGenericParams());
      if (ConstructorDecl *CD = dyn_cast<ConstructorDecl>(VD))
        printGenericParameters(CD->getGenericParams());
      if (NominalTypeDecl *NTD = dyn_cast<NominalTypeDecl>(VD))
        printGenericParameters(NTD->getGenericParams());

      OS << " type='";
      if (VD->hasType())
        VD->getType()->print(OS);
      else
        OS << "<null type>";
      OS << '\'';

      if (VD->hasFixedLifetime()) OS << " hasFixedLifetime=true";
      if (VD->isNeverUsedAsLValue()) OS << " neverUsedAsLValue=true";
    }

    void visitTranslationUnit(const TranslationUnit *TU) {
      OS.indent(Indent) << "(translation_unit";
      for (Decl *D : TU->Decls) {
        if (D->isImplicit())
          continue;

        OS << '\n';
        printRec(D);
      }
      OS << ')';
    }

    void visitVarDecl(VarDecl *VD) {
      printCommon(VD, "var_decl");
      if (VD->isProperty()) {
        if (FuncDecl *Get = VD->getGetter()) {
          OS << "\n";
          OS.indent(Indent + 2);
          OS << "get =";
          printRec(Get);
        }
        if (FuncDecl *Set = VD->getSetter()) {
          OS << "\n";
          OS.indent(Indent + 2);
          OS << "set =";
          printRec(Set);
        }
      }
      OS << ')';
    }

    void visitFuncDecl(FuncDecl *FD) {
      printCommon(FD, "func_decl", FuncColor);
      if (FD->isGetterOrSetter()) {

        if (FD->getGetterDecl()) {
          OS << " getter";
        } else {
          assert(FD->getSetterDecl() && "no getter or setter!");
          OS << " setter";
        }

        if (ValueDecl *vd = dyn_cast<ValueDecl>(FD->getGetterOrSetterDecl())) {
          OS << "_for=" << vd->getName();
        }
      }
      OS << '\n';
      printRec(FD->getBody());
      OS << ')';
    }

    void visitOneOfDecl(OneOfDecl *OOD) {
      printCommon(OOD, "oneof_decl");
      printInherited(OOD->getInherited());
      for (Decl *D : OOD->getMembers()) {
        if (D->isImplicit())
          continue;

        OS << '\n';
        printRec(D);
      }
      OS << ')';
    }

    void visitOneOfElementDecl(OneOfElementDecl *OOED) {
      printCommon(OOED, "oneof_element_decl");
      OS << ')';
    }

    void visitStructDecl(StructDecl *SD) {
      printCommon(SD, "struct_decl");
      printInherited(SD->getInherited());
      for (Decl *D : SD->getMembers()) {
        if (D->isImplicit())
          continue;

        OS << '\n';
        printRec(D);
      }
      OS << ")";
    }

    void visitClassDecl(ClassDecl *CD) {
      printCommon(CD, "class_decl");
      printInherited(CD->getInherited());
      for (Decl *D : CD->getMembers()) {
        if (D->isImplicit())
          continue;

        OS << '\n';
        printRec(D);
      }
      OS << ")";
    }

    void visitPatternBindingDecl(PatternBindingDecl *PBD) {
      printCommon(PBD, "pattern_binding_decl");
      OS << " pattern='";
      PrintPattern(OS).visit(PBD->getPattern());
      OS << '\'';
      if (PBD->getInit()) {
        OS << '\n';
        printRec(PBD->getInit());
      }
      OS << ')';
    }

    void visitSubscriptDecl(SubscriptDecl *SD) {
      printCommon(SD, "subscript_decl");
      if (FuncDecl *Get = SD->getGetter()) {
        OS << "\n";
        OS.indent(Indent + 2);
        OS << "get = ";
        printRec(Get);
      }
      if (FuncDecl *Set = SD->getSetter()) {
        OS << "\n";
        OS.indent(Indent + 2);
        OS << "set = ";
        printRec(Set);
      }
      OS << ')';
    }

    void visitConstructorDecl(ConstructorDecl *CD) {
      printCommon(CD, "constructor_decl", FuncColor);
      if (CD->getAllocThisExpr()) {
        OS << "\n";
        OS.indent(Indent+2);
        OS << "this = ";
        printRec(CD->getAllocThisExpr(), /*NoIndent=*/true);
      }
      if (CD->getBody()) {
        OS << '\n';
        printRec(CD->getBody());
      }
      OS << ')';
    }

    void visitDestructorDecl(DestructorDecl *DD) {
      printCommon(DD, "destructor_decl");
      OS << '\n';
      printRec(DD->getBody());
      OS << ')';
    }

    void visitTopLevelCodeDecl(TopLevelCodeDecl *TLCD) {
      printCommon(TLCD, "top_level_code_decl");
      if (TLCD->getBody()) {
        OS << "\n";
        printRec(TLCD->getBody());
      }
    }
    
    void visitInfixOperatorDecl(InfixOperatorDecl *IOD) {
      printCommon(IOD, "infix_operator_decl ");
      OS << IOD->getName() << "\n";
      OS.indent(Indent+2);
      OS << "associativity ";
      switch (IOD->getInfixData().getAssociativity()) {
      case Associativity::None: OS << "none\n"; break;
      case Associativity::Left: OS << "left\n"; break;
      case Associativity::Right: OS << "right\n"; break;
      }
      OS.indent(Indent+2);
      OS << "precedence " << IOD->getInfixData().getPrecedence() << ')';
    }
    
    void visitPrefixOperatorDecl(PrefixOperatorDecl *POD) {
      printCommon(POD, "prefix_operator_decl ");
      OS << POD->getName() << ')';
    }

    void visitPostfixOperatorDecl(PostfixOperatorDecl *POD) {
      printCommon(POD, "postfix_operator_decl ");
      OS << POD->getName() << ')';
    }
  };
} // end anonymous namespace.

void Decl::dump() const {
  print(llvm::errs());
  int TmpBuf = -1, TmpLine = -1;
  PrintDecl(llvm::errs(), 0, TmpBuf, TmpLine,
            getASTContext().SourceMgr).visit(const_cast<Decl *>(this));
  llvm::errs() << '\n';
}

void TranslationUnit::dump() const {
  int TmpBuf = -1, TmpLine = -1;
  PrintDecl(llvm::errs(), 0, TmpBuf, TmpLine,
            getASTContext().SourceMgr).visitTranslationUnit(this);
  llvm::errs() << '\n';
}

void Pattern::print(llvm::raw_ostream &OS) const {
  PrintPattern(OS).visit(const_cast<Pattern*>(this));
}

void Pattern::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}

//===----------------------------------------------------------------------===//
// Printing for Stmt and all subclasses.
//===----------------------------------------------------------------------===//

namespace {
/// PrintStmt - Visitor implementation of Expr::print.
class PrintStmt : public StmtVisitor<PrintStmt> {
public:
  raw_ostream &OS;
  unsigned Indent;
  int &LastBuffer;
  int &LastLine;
  const llvm::SourceMgr *SourceMgr;
  bool ShowColors;

  PrintStmt(raw_ostream &os, unsigned indent, int &lastBuffer, int &lastLine,
            const llvm::SourceMgr *SM)
    : OS(os), Indent(indent), LastBuffer(lastBuffer), LastLine(lastLine),
      SourceMgr(SM) {
    ShowColors = OS.has_colors();
  }

  void printRec(Stmt *S) {
    Indent += 2;
    if (S)
      visit(S);
    else
      OS.indent(Indent) << "(**NULL STATEMENT**)";
    Indent -= 2;
  }

  void printRec(Decl *D) {
    PrintDecl(OS, Indent+2, LastBuffer, LastLine,
              D->getASTContext().SourceMgr).visit(D);
  }
  void printRec(Expr *E);
  void printRec(Pattern *P) { P->print(OS); }

  raw_ostream &printCommon(Stmt *S, const char *C) {
    OS.indent(Indent) << '(' << C;
    if (SourceMgr) {
      OS << ' ';
      if (ShowColors) {
        if (const char *CStr =
            llvm::sys::Process::OutputColor(SourceRangeColor, false, false)) {
          OS << CStr;
        }
      }
      S->getSourceRange().print(OS, *SourceMgr, LastBuffer, LastLine,
                                /*PrintText=*/false);
      if (ShowColors)
        OS << llvm::sys::Process::ResetColor();
    }
    return OS;
  }

  void visitBraceStmt(BraceStmt *S) {
    printCommon(S, "brace_stmt");
    for (auto Elt : S->getElements()) {
      OS << '\n';
      if (Expr *SubExpr = Elt.dyn_cast<Expr*>())
        printRec(SubExpr);
      else if (Stmt *SubStmt = Elt.dyn_cast<Stmt*>())
        printRec(SubStmt);
      else
        printRec(Elt.get<Decl*>());
    }
    OS << ')';
  }

  void visitReturnStmt(ReturnStmt *S) {
    printCommon(S, "return_stmt");
    if (S->hasResult()) {
      OS << '\n';
      printRec(S->getResult());
    }
    OS << ')';
  }

  void visitIfStmt(IfStmt *S) {
    printCommon(S, "if_stmt") << '\n';
    printRec(S->getCond());
    OS << '\n';
    printRec(S->getThenStmt());
    if (S->getElseStmt()) {
      OS << '\n';
      printRec(S->getElseStmt());
    }
    OS << ')';
  }
  void visitWhileStmt(WhileStmt *S) {
    printCommon(S, "while_stmt") << '\n';
    printRec(S->getCond());
    OS << '\n';
    printRec(S->getBody());
    OS << ')';
  }

  void visitDoWhileStmt(DoWhileStmt *S) {
    printCommon(S, "do_while_stmt") << '\n';
    printRec(S->getBody());
    OS << '\n';
    printRec(S->getCond());
    OS << ')';
  }
  void visitForStmt(ForStmt *S) {
    printCommon(S, "for_stmt") << '\n';
    if (!S->getInitializerVarDecls().empty()) {
      for (auto D : S->getInitializerVarDecls()) {
        printRec(D);
        OS << '\n';
      }
    } else if (S->getInitializer()) {
      printRec(S->getInitializer());
      OS << '\n';
    } else {
      OS.indent(Indent+2) << "<null initializer>\n";
    }

    if (S->getCond().isNull())
      OS.indent(Indent+2) << "<null condition>";
    else
      printRec(S->getCond().get());
    OS << '\n';

    if (S->getIncrement()) {
      printRec(S->getIncrement());
    } else {
      OS.indent(Indent+2) << "<null increment>";
    }
    OS << '\n';
    printRec(S->getBody());
    OS << ')';
  }
  void visitForEachStmt(ForEachStmt *S) {
    printCommon(S, "for_each_stmt") << '\n';
    printRec(S->getPattern());
    OS << '\n';
    printRec(S->getContainer());
    OS << '\n';
    printRec(S->getBody());
    OS << ')';
  }
  void visitBreakStmt(BreakStmt *S) {
    printCommon(S, "break_stmt") << ')';
  }
  void visitContinueStmt(ContinueStmt *S) {
    printCommon(S, "continue_stmt") << ')';
  }
  void visitFallthroughStmt(FallthroughStmt *S) {
    printCommon(S, "fallthrough_stmt") << ')';
  }
  void visitSwitchStmt(SwitchStmt *S) {
    printCommon(S, "switch_stmt") << '\n';
    printRec(S->getSubjectExpr());
    for (CaseStmt *C : S->getCases()) {
      OS << '\n';
      printRec(C);
    }
    OS << ')';
  }
  void visitCaseStmt(CaseStmt *S) {
    printCommon(S, "case_stmt");
    for (CaseLabel *label : S->getCaseLabels()) {
      OS << '\n';
      OS.indent(Indent+2) << "(case_label";
      for (Pattern *p : label->getPatterns()) {
        OS << '\n';
        OS.indent(Indent+4);
        p->print(OS);
      }
      if (Expr *guard = label->getGuardExpr()) {
        OS << '\n';
        printRec(guard);
      }
      OS << ')';
    }
    OS << '\n';
    printRec(S->getBody());
    OS << ')';
  }
};

} // end anonymous namespace.

void Stmt::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}

void Stmt::print(raw_ostream &OS, unsigned Indent) const {
  int TmpBuf = -1, TmpLine = -1;
  PrintStmt(OS, Indent, TmpBuf, TmpLine, nullptr).visit(const_cast<Stmt*>(this));
}

//===----------------------------------------------------------------------===//
// Printing for Expr and all subclasses.
//===----------------------------------------------------------------------===//

namespace {
/// PrintExpr - Visitor implementation of Expr::print.
class PrintExpr : public ExprVisitor<PrintExpr> {
public:
  raw_ostream &OS;
  unsigned Indent;
  int &LastBuffer;
  int &LastLine;
  const llvm::SourceMgr *SourceMgr;
  bool ShowColors;

  PrintExpr(raw_ostream &os, unsigned indent, int &lastBuffer, int &lastLine,
            const llvm::SourceMgr *SM)
    : OS(os), Indent(indent), LastBuffer(lastBuffer), LastLine(lastLine),
      SourceMgr(SM) {
    ShowColors = OS.has_colors();
  }

  void printRec(Expr *E) {
    Indent += 2;
    if (E)
      visit(E);
    else
      OS.indent(Indent) << "(**NULL EXPRESSION**)";
    Indent -= 2;
  }

  /// FIXME: This should use ExprWalker to print children.

  void printRec(Decl *D) {
    PrintDecl(OS, Indent+2, LastBuffer, LastLine,
              D->getASTContext().SourceMgr).visit(D);
  }
  void printRec(Stmt *S) {
    PrintStmt(OS, Indent+2, LastBuffer, LastLine, SourceMgr).visit(S);
  }

  void printSubstitutions(ArrayRef<Substitution> Substitutions) {
    for (auto S : Substitutions) {
      OS.indent(Indent + 2) << "(with " << S.Archetype->getFullName()
                            << " = " << S.Replacement.getString() << ")\n";
    }
  }

  raw_ostream &printCommon(Expr *E, const char *C) {
    OS.indent(Indent) << '(' << C;
    if (SourceMgr) {
      OS << ' ';
      if (ShowColors) {
        if (const char *CStr =
            llvm::sys::Process::OutputColor(SourceRangeColor, false, false)) {
          OS << CStr;
        }
      }
      E->getSourceRange().print(OS, *SourceMgr, LastBuffer, LastLine,
                                /*PrintText=*/false);
      if (ShowColors)
        OS << llvm::sys::Process::ResetColor();
    }
    return OS << " type='" << E->getType() << '\'';
  }

  void visitErrorExpr(ErrorExpr *E) {
    printCommon(E, "error_expr") << ')';
  }

  void visitIntegerLiteralExpr(IntegerLiteralExpr *E) {
    printCommon(E, "integer_literal_expr") << " value=";
    if (E->getType().isNull() || E->getType()->isUnresolvedType())
      OS << E->getText();
    else
      OS << E->getValue();
    OS << ')';
  }
  void visitFloatLiteralExpr(FloatLiteralExpr *E) {
    printCommon(E, "float_literal_expr") << " value=" << E->getText() << ')';
  }
  void visitCharacterLiteralExpr(CharacterLiteralExpr *E) {
    printCommon(E, "character_literal_expr") << " value=" << E->getValue()<<')';
  }
  void visitStringLiteralExpr(StringLiteralExpr *E) {
    printCommon(E, "string_literal_expr") << " value=" << E->getValue() << ')';
  }
  void visitInterpolatedStringLiteralExpr(InterpolatedStringLiteralExpr *E) {
    printCommon(E, "interpolated_string_literal_expr");
    for (auto Segment : E->getSegments()) {
      OS << '\n';
      printRec(Segment);
    }
    OS << ')';
  }
  void visitMagicIdentifierLiteralExpr(MagicIdentifierLiteralExpr *E) {
    printCommon(E, "magic_identifier_literal_expr") << " kind=";
    switch (E->getKind()) {
    case MagicIdentifierLiteralExpr::File:  OS << "__FILE__"; break;
    case MagicIdentifierLiteralExpr::Line:  OS << "__LINE__"; break;
    case MagicIdentifierLiteralExpr::Column:  OS << "__COLUMN__"; break;
    }
    OS << ')';
  }

  void visitDeclRefExpr(DeclRefExpr *E) {
    printCommon(E, "declref_expr")
      << " decl=" << E->getDecl()->getName() << ')';
  }
  void visitSuperRefExpr(SuperRefExpr *E) {
    printCommon(E, "super_ref_expr") << ')';
  }
  void visitOtherConstructorDeclRefExpr(OtherConstructorDeclRefExpr *E) {
    printCommon(E, "other_constructor_ref_expr") << ')';
  }
  void visitUnresolvedConstructorExpr(UnresolvedConstructorExpr *E) {
    printCommon(E, "unresolved_constructor") << '\n';
    printRec(E->getSubExpr());
    OS << ')';
  }
  void visitOverloadedDeclRefExpr(OverloadedDeclRefExpr *E) {
    printCommon(E, "overloaded_decl_ref_expr")
      << " name=" << E->getDecls()[0]->getName().str()
      << " #decls=" << E->getDecls().size();
    for (ValueDecl *D : E->getDecls()) {
      OS << '\n';
      OS.indent(Indent);
      OS << "  type=" << D->getTypeOfReference().getString();
    }
    OS << ')';
  }
  void visitOverloadedMemberRefExpr(OverloadedMemberRefExpr *E) {
    printCommon(E, "overloaded_member_ref_expr")
      << " name=" << E->getDecls()[0]->getName().str()
      << " #decls=" << E->getDecls().size() << "\n";
    printRec(E->getBase());
    for (ValueDecl *D : E->getDecls()) {
      OS << '\n';
      OS.indent(Indent);
      OS << "  type=" << D->getTypeOfReference().getString();
    }
    OS << ')';
  }
  void visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *E) {
    printCommon(E, "unresolved_decl_ref_expr")
      << " name=" << E->getName() << ')';
  }
  void visitUnresolvedSpecializeExpr(UnresolvedSpecializeExpr *E) {
    printCommon(E, "unresolved_specialize_expr") << '\n';
    printRec(E->getSubExpr());
    for (TypeLoc T : E->getUnresolvedParams()) {
      OS << '\n';
      OS.indent(Indent+2);
      T.getType()->print(OS);
    }
    OS << ')';
  }

  void visitMemberRefExpr(MemberRefExpr *E) {
    printCommon(E, "member_ref_expr")
      << " decl=" << E->getDecl()->getName() << '\n';
    printRec(E->getBase());
    OS << ')';
  }
  void visitExistentialMemberRefExpr(ExistentialMemberRefExpr *E) {
    printCommon(E, "existential_member_ref_expr")
    << " decl=" << E->getDecl()->getName() << '\n';
    printRec(E->getBase());
    OS << ')';
  }
  void visitArchetypeMemberRefExpr(ArchetypeMemberRefExpr *E) {
    printCommon(E, "archetype_member_ref_expr")
      << " decl=" << E->getDecl()->getName() << '\n';
    printRec(E->getBase());
    OS << ')';
  }
  void visitGenericMemberRefExpr(GenericMemberRefExpr *E) {
    printCommon(E, "generic_member_ref_expr")
      << " decl=" << E->getDecl()->getName() << '\n';
    printSubstitutions(E->getSubstitutions());
    printRec(E->getBase());
    OS << ')';
  }
  void visitUnresolvedMemberExpr(UnresolvedMemberExpr *E) {
    printCommon(E, "unresolved_member_expr")
      << " name='" << E->getName() << "')";
  }
  void visitParenExpr(ParenExpr *E) {
    printCommon(E, "paren_expr");
    if (E->hasTrailingClosure())
      OS << " trailing-closure";
    OS << '\n';
    printRec(E->getSubExpr());
    OS << ')';
  }
  void visitTupleExpr(TupleExpr *E) {
    printCommon(E, "tuple_expr");
    if (E->hasTrailingClosure())
      OS << " trailing-closure";

    for (unsigned i = 0, e = E->getNumElements(); i != e; ++i) {
      OS << '\n';
      if (E->getElement(i))
        printRec(E->getElement(i));
      else
        OS.indent(Indent+2) << "<<tuple element default value>>";
    }
    OS << ')';
  }
  void visitArrayExpr(ArrayExpr *E) {
    printCommon(E, "array_expr");
    OS << '\n';
    printRec(E->getSubExpr());
  }
  void visitDictionaryExpr(DictionaryExpr *E) {
    printCommon(E, "dictionary_expr");
    OS << '\n';
    printRec(E->getSubExpr());
  }
  void visitSubscriptExpr(SubscriptExpr *E) {
    printCommon(E, "subscript_expr");
    OS << '\n';
    printRec(E->getBase());
    OS << '\n';
    printRec(E->getIndex());
    OS << ')';
  }
  void visitExistentialSubscriptExpr(ExistentialSubscriptExpr *E) {
    printCommon(E, "existential_subscript_expr");
    OS << '\n';
    printRec(E->getBase());
    OS << '\n';
    printRec(E->getIndex());
    OS << ')';
  }
  void visitArchetypeSubscriptExpr(ArchetypeSubscriptExpr *E) {
    printCommon(E, "archetype_subscript_expr");
    OS << '\n';
    printRec(E->getBase());
    OS << '\n';
    printRec(E->getIndex());
    OS << ')';
  }
  void visitGenericSubscriptExpr(GenericSubscriptExpr *E) {
    printCommon(E, "generic_subscript_expr");
    OS << '\n';
    printSubstitutions(E->getSubstitutions());
    printRec(E->getBase());
    OS << '\n';
    printRec(E->getIndex());
    OS << ')';
  }
  void visitUnresolvedDotExpr(UnresolvedDotExpr *E) {
    printCommon(E, "unresolved_dot_expr")
      << " field '" << E->getName().str() << "'";
    if (E->getBase()) {
      OS << '\n';
      printRec(E->getBase());
    }
    OS << ')';
  }
  void visitModuleExpr(ModuleExpr *E) {
    printCommon(E, "module_expr") << ')';
  }
  void visitTupleElementExpr(TupleElementExpr *E) {
    printCommon(E, "tuple_element_expr")
      << " field #" << E->getFieldNumber() << '\n';
    printRec(E->getBase());
    OS << ')';
  }
  void visitTupleShuffleExpr(TupleShuffleExpr *E) {
    printCommon(E, "tuple_shuffle_expr") << " elements=[";
    for (unsigned i = 0, e = E->getElementMapping().size(); i != e; ++i) {
      if (i) OS << ", ";
      OS << E->getElementMapping()[i];
    }
    OS << "]\n";
    printRec(E->getSubExpr());
    OS << ')';
  }
  void visitFunctionConversionExpr(FunctionConversionExpr *E) {
    printCommon(E, "function_conversion_expr") << '\n';
    printRec(E->getSubExpr());
    OS << ')';
  }
  void visitErasureExpr(ErasureExpr *E) {
    printCommon(E, "erasure_expr") << '\n';
    printRec(E->getSubExpr());
    OS << ')';
  }
  void visitSpecializeExpr(SpecializeExpr *E) {
    printCommon(E, "specialize_expr") << '\n';
    printSubstitutions(E->getSubstitutions());
    printRec(E->getSubExpr());
    OS << ')';
  }
  void visitLoadExpr(LoadExpr *E) {
    printCommon(E, "load_expr") << '\n';
    printRec(E->getSubExpr());
    OS << ')';
  }
  void visitMaterializeExpr(MaterializeExpr *E) {
    printCommon(E, "materialize_expr") << '\n';
    printRec(E->getSubExpr());
    OS << ')';
  }
  void visitRequalifyExpr(RequalifyExpr *E) {
    printCommon(E, "requalify_expr") << '\n';
    printRec(E->getSubExpr());
    OS << ')';
  }
  void visitMetatypeConversionExpr(MetatypeConversionExpr *E) {
    printCommon(E, "metatype_conversion_expr") << '\n';
    printRec(E->getSubExpr());
    OS << ')';
  }
  void visitDerivedToBaseExpr(DerivedToBaseExpr *E) {
    printCommon(E, "derived_to_base_expr") << '\n';
    printRec(E->getSubExpr());
    OS << ')';
  }
  void visitArchetypeToSuperExpr(ArchetypeToSuperExpr *E) {
    printCommon(E, "archetype_to_super_expr") << '\n';
    printRec(E->getSubExpr());
    OS << ')';
  }
  void visitScalarToTupleExpr(ScalarToTupleExpr *E) {
    printCommon(E, "scalar_to_tuple_expr");
    OS << " field=" << E->getScalarField();
    OS << '\n';
    printRec(E->getSubExpr());
    OS << ')';
  }
  void visitBridgeToBlockExpr(BridgeToBlockExpr *E) {
    printCommon(E, "bridge_to_block") << '\n';
    printRec(E->getSubExpr());
    OS << ')';
  }

  void visitAddressOfExpr(AddressOfExpr *E) {
    printCommon(E, "address_of_expr") << '\n';
    printRec(E->getSubExpr());
    OS << ')';
  }
  void visitSequenceExpr(SequenceExpr *E) {
    printCommon(E, "sequence_expr");
    for (unsigned i = 0, e = E->getNumElements(); i != e; ++i) {
      OS << '\n';
      printRec(E->getElement(i));
    }
    OS << ')';
  }

  llvm::raw_ostream &printCapturing(CapturingExpr *E, char const *name) {
    printCommon(E, name);
    if (!E->getCaptures().empty()) {
      OS << " captures=(";
      OS << E->getCaptures()[0]->getName();
      for (auto capture : E->getCaptures().slice(1)) {
        OS << ", " << capture->getName();
      }
      OS << ')';
    }
    return OS;
  }

  void visitFuncExpr(FuncExpr *E) {
    printCapturing(E, "func_expr");
    if (E->getBody()) {
      OS << '\n';
      printRec(E->getBody());
    }
    OS << ')';
  }
  void visitPipeClosureExpr(PipeClosureExpr *expr) {
    printCapturing(expr, "closure_expr");
    if (expr->hasSingleExpressionBody()) {
      OS << " single-expression\n";
      printRec(expr->getSingleExpressionBody());
    } else
      printRec(expr->getBody());
    OS << ')';
  }
  void visitImplicitClosureExpr(ImplicitClosureExpr *E) {
    printCapturing(E, "implicit_closure_expr") << '\n';
    printRec(E->getBody());
    OS << ')';
  }

  void visitNewArrayExpr(NewArrayExpr *E) {
    printCommon(E, "new_array_expr")
      << " elementType='" << E->getElementTypeLoc().getType() << "'";
    OS << '\n';
    if (E->hasInjectionFunction())
      printRec(E->getInjectionFunction());
    for (auto &bound : E->getBounds()) {
      OS << '\n';
      if (bound.Value)
        printRec(bound.Value);
      else
        OS.indent(Indent + 2) << "(empty bound)";
    }
    OS << ')';
  }

  void visitMetatypeExpr(MetatypeExpr *E) {
    printCommon(E, "metatype_expr");
    if (Expr *base = E->getBase()) {
      OS << '\n';
      printRec(base);
    } else {
      OS << " baseless";
    }
    OS << ")";
  }

  void visitOpaqueValueExpr(OpaqueValueExpr *E) {
    printCommon(E, "opaque_value_expr") << ')';
  }

  void visitZeroValueExpr(ZeroValueExpr *E) {
    printCommon(E, "zero_value_expr") << ')';
  }

  void printApplyExpr(ApplyExpr *E, const char *NodeName) {
    printCommon(E, NodeName);
    if (E->isSuper())
      OS << " super";
    OS << '\n';
    printRec(E->getFn());
    OS << '\n';
    printRec(E->getArg());
    OS << ')';
  }

  void visitCallExpr(CallExpr *E) {
    printApplyExpr(E, "call_expr");
  }
  void visitPrefixUnaryExpr(PrefixUnaryExpr *E) {
    printApplyExpr(E, "prefix_unary_expr");
  }
  void visitPostfixUnaryExpr(PostfixUnaryExpr *E) {
    printApplyExpr(E, "postfix_unary_expr");
  }
  void visitBinaryExpr(BinaryExpr *E) {
    printApplyExpr(E, "binary_expr");
  }
  void visitDotSyntaxCallExpr(DotSyntaxCallExpr *E) {
    printApplyExpr(E, "dot_syntax_call_expr");
  }
  void visitConstructorRefCallExpr(ConstructorRefCallExpr *E) {
    printApplyExpr(E, "constructor_ref_call_expr");
  }
  void visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E) {
    printCommon(E, "dot_syntax_base_ignored") << '\n';
    printRec(E->getLHS());
    OS << '\n';
    printRec(E->getRHS());
    OS << ')';
  }

  void printExplicitCastExpr(ExplicitCastExpr *E, const char *name) {
    printCommon(E, name) << ' ';
    E->getCastTypeLoc().getType()->print(OS);
    OS << '\n';
    printRec(E->getSubExpr());
    OS << ')';
  }

  void visitCoerceExpr(CoerceExpr *E) {
    printExplicitCastExpr(E, "coerce_expr");
  }
  void visitUnconditionalCheckedCastExpr(UnconditionalCheckedCastExpr *E) {
    printExplicitCastExpr(E, "unconditional_checked_cast_expr");
  }
  void visitIsaExpr(IsaExpr *E) {
    printExplicitCastExpr(E, "is_subtype_expr");
  }
  void visitRebindThisInConstructorExpr(RebindThisInConstructorExpr *E) {
    printCommon(E, "rebind_this_in_constructor_expr") << '\n';
    printRec(E->getSubExpr());
    OS << ')';
  }
  void visitIfExpr(IfExpr *E) {
    printCommon(E, "if_expr") << '\n';
    printRec(E->getCondExpr());
    OS << '\n';
    printRec(E->getThenExpr());
    OS << '\n';
    printRec(E->getElseExpr());
    OS << ')';
  }
  void visitDefaultValueExpr(DefaultValueExpr *E) {
    printCommon(E, "default_value_expr") << ' ';
    printRec(E->getSubExpr());
    OS << ')';
  }
  void visitAssignExpr(AssignExpr *E) {
    OS.indent(Indent) << "(assign_expr\n";
    printRec(E->getDest());
    OS << '\n';
    printRec(E->getSrc());
    OS << ')';
  }
  void visitUnresolvedPatternExpr(UnresolvedPatternExpr *E) {
    OS.indent(Indent) << "(unresolved_pattern_expr ";
    E->getSubPattern()->print(OS);
    OS << ')';
  }
};

} // end anonymous namespace.

void PrintDecl::printRec(Expr *E, bool NoIndent) {
  PrintExpr(OS, NoIndent ? 0 : Indent+2,
            LastBuffer, LastLine, &SourceMgr).visit(E);
}
void PrintDecl::printRec(Stmt *S) {
  PrintStmt(OS, Indent+2, LastBuffer, LastLine, &SourceMgr).visit(S);
}

void PrintStmt::printRec(Expr *E) {
  PrintExpr(OS, Indent+2, LastBuffer, LastLine, SourceMgr).visit(E);
}

void Expr::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}

void Expr::print(raw_ostream &OS, unsigned Indent) const {
  int TmpBuf = -1, TmpLine = -1;
  PrintExpr(OS, Indent, TmpBuf, TmpLine, nullptr).visit(const_cast<Expr*>(this));
}
