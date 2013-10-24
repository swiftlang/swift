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
#include "swift/Basic/STLExtras.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/Process.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

#define DEF_COLOR(NAME, COLOR)\
static const llvm::raw_ostream::Colors NAME##Color = llvm::raw_ostream::COLOR;

DEF_COLOR(Func, YELLOW)
DEF_COLOR(Extension, MAGENTA)
DEF_COLOR(Pattern, RED)
DEF_COLOR(TypeRepr, GREEN)

#undef DEF_COLOR

//===----------------------------------------------------------------------===//
//  Decl printing.
//===----------------------------------------------------------------------===//

namespace {
  class PrintPattern : public PatternVisitor<PrintPattern> {
  public:
    raw_ostream &OS;
    unsigned Indent;
    bool ShowColors;

    explicit PrintPattern(raw_ostream &os, unsigned indent = 0)
      : OS(os), Indent(indent), ShowColors(false) {
      if (&os == &llvm::errs() || &os == &llvm::outs())
        ShowColors = llvm::errs().has_colors() && llvm::outs().has_colors();
    }

    void printRec(Decl *D) { D->dump(OS, Indent + 2); }
    void printRec(Expr *E) { E->print(OS, Indent + 2); }
    void printRec(Stmt *S) { S->print(OS, Indent + 2); }
    void printRec(TypeRepr *T);
    void printRec(Pattern *P) { PrintPattern(OS, Indent+2).visit(P); }

    raw_ostream &printCommon(Pattern *P, const char *Name) {
      OS.indent(Indent) << '(';

      // Support optional color output.
      if (ShowColors) {
        if (const char *CStr =
            llvm::sys::Process::OutputColor(PatternColor, false, false)) {
          OS << CStr;
        }
      }

      OS << Name;

      if (ShowColors)
        OS << llvm::sys::Process::ResetColor();

      if (P->isImplicit())
        OS << " implicit";

      if (P->hasType()) {
        OS << " type='";
        P->getType().print(OS);
        OS << '\'';
      }
      return OS;
    }

    void visitParenPattern(ParenPattern *P) {
      printCommon(P, "pattern_paren") << '\n';
      printRec(P->getSubPattern());
      OS << ')';
    }
    void visitTuplePattern(TuplePattern *P) {
      printCommon(P, "pattern_tuple");
      if (P->hasVararg())
        OS << " hasVararg";
      for (unsigned i = 0, e = P->getNumFields(); i != e; ++i) {
        OS << '\n';
        printRec(P->getFields()[i].getPattern());
        if (P->getFields()[i].getInit()) {
          OS << '\n';
          printRec(P->getFields()[i].getInit()->getExpr());
        }
      }
      OS << ')';
    }
    void visitNamedPattern(NamedPattern *P) {
      printCommon(P, "pattern_named")<< " '" << P->getBoundName().str() << "')";
    }
    void visitAnyPattern(AnyPattern *P) {
      printCommon(P, "pattern_any") << ')';
    }
    void visitTypedPattern(TypedPattern *P) {
      printCommon(P, "pattern_typed") << '\n';
      printRec(P->getSubPattern());
      if (P->getTypeLoc().getTypeRepr()) {
        OS << '\n';
        printRec(P->getTypeLoc().getTypeRepr());
      }
      OS << ')';
    }
    
    void visitIsaPattern(IsaPattern *P) {
      printCommon(P, "pattern_isa") << ' ';
      P->getCastTypeLoc().getType().print(OS);
      OS << ')';
    }
    void visitNominalTypePattern(NominalTypePattern *P) {
      printCommon(P, "pattern_nominal") << ' ';
      P->getCastTypeLoc().getType().print(OS);
      // FIXME: We aren't const-correct.
      for (auto &elt : P->getMutableElements()) {
        OS << '\n';
        OS.indent(Indent) << elt.getPropertyName() << ": ";
        printRec(elt.getSubPattern());
      }
      OS << ')';
    }
    void visitExprPattern(ExprPattern *P) {
      printCommon(P, "pattern_expr");
      OS << '\n';
      printRec(P->getSubExpr());
      OS << ')';
    }
    void visitVarPattern(VarPattern *P) {
      printCommon(P, "pattern_var");
      OS << '\n';
      printRec(P->getSubPattern());
      OS << ')';
    }
    void visitEnumElementPattern(EnumElementPattern *P) {
      printCommon(P, "pattern_enum_element");
      OS << ' ';
      P->getParentType().getType().print(OS);
      OS << '.' << P->getName();
      if (P->hasSubPattern()) {
        OS << '\n';
        printRec(P->getSubPattern());
      }
      OS << ')';
    }
  };

  /// PrintDecl - Visitor implementation of Decl::print.
  class PrintDecl : public DeclVisitor<PrintDecl> {
  public:
    raw_ostream &OS;
    unsigned Indent;
    bool ShowColors;

    explicit PrintDecl(raw_ostream &os, unsigned indent = 0)
      : OS(os), Indent(indent), ShowColors(false) {
      if (&os == &llvm::errs() || &os == &llvm::outs())
        ShowColors = llvm::errs().has_colors() && llvm::outs().has_colors();
    }
    
    void printRec(Decl *D) { PrintDecl(OS, Indent + 2).visit(D); }
    void printRec(Expr *E) { E->print(OS, Indent+2); }
    void printRec(Stmt *S) { S->print(OS, Indent+2); }
    void printRec(Pattern *P) { PrintPattern(OS, Indent+2).visit(P); }
    void printRec(TypeRepr *T);

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
          P.getAsTypeParam()->getInherited()[0].getType().print(OS);
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

        Super.getType().print(OS);
      }
    }

    void visitImportDecl(ImportDecl *ID) {
      printCommon(ID, "import_decl");

      if (ID->isExported())
        OS << " exported";

      const char *KindString;
      switch (ID->getImportKind()) {
      case ImportKind::Module:
        KindString = nullptr;
        break;
      case ImportKind::Type:
        KindString = "type";
        break;
      case ImportKind::Struct:
        KindString = "struct";
        break;
      case ImportKind::Class:
        KindString = "class";
        break;
      case ImportKind::Enum:
        KindString = "enum";
        break;
      case ImportKind::Protocol:
        KindString = "protocol";
        break;
      case ImportKind::Var:
        KindString = "var";
        break;
      case ImportKind::Func:
        KindString = "func";
        break;
      }
      if (KindString)
        OS << " kind=" << KindString;

      OS << " ";
      interleave(ID->getFullAccessPath(),
                 [&](const ImportDecl::AccessPathElement &Elem) {
                   OS << Elem.first;
                 },
                 [&] { OS << '.'; });
      OS << "')";
    }

    void visitExtensionDecl(ExtensionDecl *ED) {
      printCommon(ED, "extension_decl", ExtensionColor);
      OS << ' ';
      ED->getExtendedType().print(OS);
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
        OS << '\"' << D->getName() << '\"';
      else
        OS << "'anonname=" << (const void*)D << '\'';
    }

    void visitTypeAliasDecl(TypeAliasDecl *TAD) {
      printCommon(TAD, "typealias");
      OS << " type='";
      if (TAD->hasUnderlyingType())
        TAD->getUnderlyingType().print(OS);
      else
        OS << "<<<unresolved>>>";
      printInherited(TAD->getInherited());
      OS << "')";
    }

    void visitGenericTypeParamDecl(GenericTypeParamDecl *decl) {
      printCommon(decl, "generic_type_param");
      OS << " depth=" << decl->getDepth() << " index=" << decl->getIndex();
      OS << ")";
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

    void printCommon(ValueDecl *VD, const char *Name,
                     llvm::Optional<llvm::raw_ostream::Colors> Color =
                      llvm::Optional<llvm::raw_ostream::Colors>()) {
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
        VD->getType().print(OS);
      else
        OS << "<null type>";
      OS << '\'';

      if (VD->conformsToProtocolRequirement())
        OS << " conforms";
      if (auto Overridden = VD->getOverriddenDecl()) {
        OS << " override=";
        Overridden->dumpRef(OS);
      }
    }

    void visitSourceFile(const SourceFile &SF) {
      OS.indent(Indent) << "(source_file";
      for (Decl *D : SF.Decls) {
        if (D->isImplicit())
          continue;

        OS << '\n';
        printRec(D);
      }
      OS << ')';
    }

    void visitVarDecl(VarDecl *VD) {
      printCommon(VD, "var_decl");
      if (VD->isComputed()) {
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

    void visitEnumDecl(EnumDecl *UD) {
      printCommon(UD, "enum_decl");
      printInherited(UD->getInherited());
      for (Decl *D : UD->getMembers()) {
        if (D->isImplicit())
          continue;

        OS << '\n';
        printRec(D);
      }
      OS << ')';
    }

    void visitEnumElementDecl(EnumElementDecl *UED) {
      printCommon(UED, "enum_element_decl");
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
      OS << '\n';
      printRec(PBD->getPattern());
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
    
    void printCommonAFD(AbstractFunctionDecl *D, const char *Type) {
      printCommon(D, Type, FuncColor);
      if (!D->getCaptureInfo().empty()) {
        OS << " ";
        D->getCaptureInfo().print(OS);
      }
      if (D->getInterfaceType()) {
        OS << "\n";
        OS.indent(Indent + 2);
        OS << "interface type=";
        D->getInterfaceType().print(OS);
      }
    }

    void printPatterns(StringRef Text, ArrayRef<Pattern*> Pats) {
      if (Pats.empty())
        return;
      if (!Text.empty()) {
        OS << '\n';
        Indent += 2;
        OS.indent(Indent) << '(' << Text;
      }
      for (auto P : Pats) {
        OS << '\n';
        printRec(P);
      }
      if (!Text.empty()) {
        OS << ')';
        Indent -= 2;
      }
    }

    void printAbstractFunctionDecl(AbstractFunctionDecl *D) {
      if (D->hasSelectorStyleSignature()) {
        printPatterns("arg_params", D->getArgParamPatterns());
        printPatterns("body_params", D->getBodyParamPatterns());
      } else {
        printPatterns(StringRef(), D->getBodyParamPatterns());
      }
      if (auto FD = dyn_cast<FuncDecl>(D)) {
        if (FD->getBodyResultTypeLoc().getTypeRepr()) {
          OS << '\n';
          Indent += 2;
          OS.indent(Indent);
          OS << "(result\n";
          printRec(FD->getBodyResultTypeLoc().getTypeRepr());
          OS << ')';
          Indent -= 2;
        }
      }
      if (auto Body = D->getBody()) {
        OS << '\n';
        printRec(Body);
      }
     }
    
    void visitFuncDecl(FuncDecl *FD) {
      printCommonAFD(FD, "func_decl");
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
      
      for (auto VD: FD->getConformances()) {
        OS << '\n';
        OS.indent(Indent+2) << "(conformance ";
        VD->dumpRef(OS);
        OS << ')';
      }

      printAbstractFunctionDecl(FD);

      OS << ')';
     }

    void visitConstructorDecl(ConstructorDecl *CD) {
      printCommonAFD(CD, "constructor_decl");
      printAbstractFunctionDecl(CD);
      OS << ')';
    }

    void visitDestructorDecl(DestructorDecl *DD) {
      printCommonAFD(DD, "destructor_decl");
      OS << '\n';
      printAbstractFunctionDecl(DD);
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
      switch (IOD->getAssociativity()) {
      case Associativity::None: OS << "none\n"; break;
      case Associativity::Left: OS << "left\n"; break;
      case Associativity::Right: OS << "right\n"; break;
      }
      OS.indent(Indent+2);
      OS << "precedence " << IOD->getPrecedence() << ')';
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
  dump(llvm::errs(), 0);
}

void Decl::dump(raw_ostream &OS, unsigned Indent) const {
  PrintDecl(OS, Indent).visit(const_cast<Decl *>(this));
  llvm::errs() << '\n';
}

// Print a name.
static void printName(raw_ostream &os, Identifier name) {
  if (name.empty())
    os << "<anonymous>";
  else
    os << name.str();
}

/// Print the given declaration context (with its parents).
static void printContext(raw_ostream &os, DeclContext *dc) {
  if (auto parent = dc->getParent()) {
    printContext(os, parent);
    os << '.';
  }

  switch (dc->getContextKind()) {
  case DeclContextKind::Module:
    printName(os, cast<Module>(dc)->Name);
    break;

  case DeclContextKind::SourceFile:
    // FIXME: print the file's basename?
    os << "source file";
    break;

  case DeclContextKind::AbstractClosureExpr: {
    auto *ACE = cast<AbstractClosureExpr>(dc);
    if (isa<ClosureExpr>(ACE))
      os << "explicit closure";
    if (isa<AutoClosureExpr>(ACE))
      os << "auto_closure";
    break;
  }

  case DeclContextKind::NominalTypeDecl:
    printName(os, cast<NominalTypeDecl>(dc)->getName());
    break;

  case DeclContextKind::ExtensionDecl:
    if (auto extendedTy = cast<ExtensionDecl>(dc)->getExtendedType()) {
      if (auto nominal = extendedTy->getAnyNominal()) {
        printName(os, nominal->getName());
        break;
      }
    }
    os << "extension";
    break;

  case DeclContextKind::TopLevelCodeDecl:
    os << "top-level code";
    break;

  case DeclContextKind::AbstractFunctionDecl: {
    auto *AFD = cast<AbstractFunctionDecl>(dc);
    if (isa<FuncDecl>(AFD))
      os << "func decl";
    if (isa<ConstructorDecl>(AFD))
      os << "init";
    if (isa<DestructorDecl>(AFD))
      os << "destructor";
    break;
  }
  }
}

void ValueDecl::dumpRef(raw_ostream &os) const {
  // Print the context.
  printContext(os, getDeclContext());
  os << ".";

  // Print name.
  printName(os, getName());

  // Print location.
  auto &srcMgr = getASTContext().SourceMgr;
  if (getLoc().isValid()) {
    os << '@';
    getLoc().print(os, srcMgr);
  }
}

void LLVM_ATTRIBUTE_USED ValueDecl::dumpRef() const {
  dumpRef(llvm::errs());
}

void SourceFile::dump() const {
  dump(llvm::errs());
}

void SourceFile::dump(llvm::raw_ostream &OS) const {
  PrintDecl(OS).visitSourceFile(*this);
  llvm::errs() << '\n';
}

void Pattern::dump() const {
  PrintPattern(llvm::errs()).visit(const_cast<Pattern*>(this));
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

  PrintStmt(raw_ostream &os, unsigned indent) : OS(os), Indent(indent) {
  }

  void printRec(Stmt *S) {
    Indent += 2;
    if (S)
      visit(S);
    else
      OS.indent(Indent) << "(**NULL STATEMENT**)";
    Indent -= 2;
  }

  void printRec(Decl *D) { D->dump(OS, Indent + 2); }
  void printRec(Expr *E) { E->print(OS, Indent + 2); }
  void printRec(Pattern *P) { P->print(OS); }

  void visitBraceStmt(BraceStmt *S) {
    OS.indent(Indent) << "(brace_stmt";
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
    OS.indent(Indent) << "(return_stmt";
    if (S->hasResult()) {
      OS << '\n';
      printRec(S->getResult());
    }
    OS << ')';
  }

  void visitIfStmt(IfStmt *S) {
    OS.indent(Indent) << "(if_stmt\n";
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
    OS.indent(Indent) << "(while_stmt\n";
    printRec(S->getCond());
    OS << '\n';
    printRec(S->getBody());
    OS << ')';
  }

  void visitDoWhileStmt(DoWhileStmt *S) {
    OS.indent(Indent) << "(do_while_stmt\n";
    printRec(S->getBody());
    OS << '\n';
    printRec(S->getCond());
    OS << ')';
  }
  void visitForStmt(ForStmt *S) {
    OS.indent(Indent) << "(for_stmt\n";
    if (!S->getInitializerVarDecls().empty()) {
      for (auto D : S->getInitializerVarDecls()) {
        printRec(D);
        OS << '\n';
      }
    } else if (auto *Initializer = S->getInitializer().getPtrOrNull()) {
      printRec(Initializer);
      OS << '\n';
    } else {
      OS.indent(Indent+2) << "<null initializer>\n";
    }

    if (auto *Cond = S->getCond().getPtrOrNull())
      printRec(Cond);
    else
      OS.indent(Indent+2) << "<null condition>";
    OS << '\n';

    if (auto *Increment = S->getIncrement().getPtrOrNull()) {
      printRec(Increment);
    } else {
      OS.indent(Indent+2) << "<null increment>";
    }
    OS << '\n';
    printRec(S->getBody());
    OS << ')';
  }
  void visitForEachStmt(ForEachStmt *S) {
    OS.indent(Indent) << "(for_each_stmt\n";
    printRec(S->getPattern());
    OS << '\n';
    printRec(S->getContainer());
    OS << '\n';
    printRec(S->getBody());
    OS << ')';
  }
  void visitBreakStmt(BreakStmt *S) {
    OS.indent(Indent) << "(break_stmt)";
  }
  void visitContinueStmt(ContinueStmt *S) {
    OS.indent(Indent) << "(continue_stmt)";
  }
  void visitFallthroughStmt(FallthroughStmt *S) {
    OS.indent(Indent) << "(fallthrough_stmt)";
  }
  void visitSwitchStmt(SwitchStmt *S) {
    OS.indent(Indent) << "(switch_stmt\n";
    printRec(S->getSubjectExpr());
    for (CaseStmt *C : S->getCases()) {
      OS << '\n';
      printRec(C);
    }
    OS << ')';
  }
  void visitCaseStmt(CaseStmt *S) {
    OS.indent(Indent) << "(case_stmt";
    for (CaseLabel *label : S->getCaseLabels()) {
      OS << '\n';
      OS.indent(Indent+2) << "(case_label";
      for (Pattern *p : label->getPatterns()) {
        OS << '\n';
        printRec(p);
      }
      if (Expr *guard = label->getGuardExpr()) {
        OS << '\n';
        guard->print(OS, Indent+4);
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
  PrintStmt(OS, Indent).visit(const_cast<Stmt*>(this));
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

  PrintExpr(raw_ostream &os, unsigned indent) : OS(os), Indent(indent) {
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

  void printRec(Decl *D) { D->dump(OS, Indent + 2); }
  void printRec(Stmt *S) { S->print(OS, Indent + 2); }
  void printRec(Pattern *P) {
    PrintPattern(OS, Indent+2).visit(P);
  }
  void printRec(TypeRepr *T);

  void printSubstitutions(ArrayRef<Substitution> Substitutions) {
    for (auto S : Substitutions) {
      OS.indent(Indent + 2) << "(with " << S.Archetype->getFullName()
                            << " = " << S.Replacement << ")\n";
    }
  }

  raw_ostream &printCommon(Expr *E, const char *C) {
    OS.indent(Indent) << '(' << C;
    if (E->isImplicit())
      OS << " implicit";
    return OS << " type='" << E->getType() << '\'';
  }

  void visitErrorExpr(ErrorExpr *E) {
    printCommon(E, "error_expr") << ')';
  }

  void visitIntegerLiteralExpr(IntegerLiteralExpr *E) {
    printCommon(E, "integer_literal_expr");
    if (E->isNegative())
      OS << " negative";
    OS << " value=";
    Type T = E->getType();
    if (T.isNull() || T->is<ErrorType>() || T->hasTypeVariable())
      OS << E->getDigitsText();
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

  void visitDiscardAssignmentExpr(DiscardAssignmentExpr *E) {
    printCommon(E, "discard_assignment_expr") << ')';
  }
  
  void visitDeclRefExpr(DeclRefExpr *E) {
    printCommon(E, "declref_expr")
      << " decl=";
    E->getDeclRef().dump(OS);
    OS << " specialized=" << (E->isSpecialized()? "yes" : "no");

    for (auto TR : E->getGenericArgs()) {
      OS << '\n';
      printRec(TR);
    }
    OS << ')';
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
      << " #decls=" << E->getDecls().size()
      << " specialized=" << (E->isSpecialized()? "yes" : "no");
    for (ValueDecl *D : E->getDecls()) {
      OS << '\n';
      OS.indent(Indent);
      D->dumpRef(OS);
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
      D->dumpRef(OS);
    }
    OS << ')';
  }
  void visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *E) {
    printCommon(E, "unresolved_decl_ref_expr")
      << " name=" << E->getName()
      << " specialized=" << (E->isSpecialized()? "yes" : "no") << ')';
  }
  void visitUnresolvedSpecializeExpr(UnresolvedSpecializeExpr *E) {
    printCommon(E, "unresolved_specialize_expr") << '\n';
    printRec(E->getSubExpr());
    for (TypeLoc T : E->getUnresolvedParams()) {
      OS << '\n';
      printRec(T.getTypeRepr());
    }
    OS << ')';
  }

  void visitMemberRefExpr(MemberRefExpr *E) {
    printCommon(E, "member_ref_expr")
      << " decl=";
    E->getMember().dump(OS);
    OS << '\n';
    printRec(E->getBase());
    OS << ')';
  }
  void visitExistentialMemberRefExpr(ExistentialMemberRefExpr *E) {
    printCommon(E, "existential_member_ref_expr")
      << " decl=";
    E->getDecl()->dumpRef(OS);
    OS << '\n';
    printRec(E->getBase());
    OS << ')';
  }
  void visitArchetypeMemberRefExpr(ArchetypeMemberRefExpr *E) {
    printCommon(E, "archetype_member_ref_expr")
      << " decl=";
    E->getDeclRef().dump(OS);
    OS << '\n';
    printRec(E->getBase());
    OS << ')';
  }
  void visitDynamicMemberRefExpr(DynamicMemberRefExpr *E) {
    printCommon(E, "dynamic_member_ref_expr")
      << " decl=";
    E->getMember().dump(OS);
    OS << '\n';
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
  void visitDynamicSubscriptExpr(DynamicSubscriptExpr *E) {
    printCommon(E, "dynamic_subscript_expr")
      << " decl=";
    E->getMember().dump(OS);
    OS << '\n';
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
  void visitInjectIntoOptionalExpr(InjectIntoOptionalExpr *E) {
    printCommon(E, "inject_into_optional") << '\n';
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

  llvm::raw_ostream &printClosure(AbstractClosureExpr *E, char const *name) {
    printCommon(E, name);
    if (!E->getCaptureInfo().empty()) {
      OS << " ";
      E->getCaptureInfo().print(OS);
    }
    return OS;
  }

  void visitClosureExpr(ClosureExpr *expr) {
    printClosure(expr, "closure_expr");
    if (expr->hasSingleExpressionBody()) {
      OS << " single-expression\n";
      printRec(expr->getSingleExpressionBody());
    } else
      printRec(expr->getBody());
    OS << ')';
  }
  void visitAutoClosureExpr(AutoClosureExpr *E) {
    printClosure(E, "auto_closure_expr") << '\n';
    printRec(E->getSingleExpressionBody());
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
    if (E->hasConstructionFunction()) {
      OS << '\n';
      printRec(E->getConstructionFunction());
    }
    OS << ')';
  }

  void visitMetatypeExpr(MetatypeExpr *E) {
    printCommon(E, "metatype_expr");
    if (Expr *base = E->getBase()) {
      OS << '\n';
      printRec(base);
    } else if (TypeRepr *tyR = E->getBaseTypeRepr()) {
      OS << '\n';
      printRec(tyR);
    } else {
      OS << " baseless";
    }
    OS << ")";
  }

  void visitOpaqueValueExpr(OpaqueValueExpr *E) {
    printCommon(E, "opaque_value_expr") << " @ " << (void*)E;
    if (E->isUniquelyReferenced())
      OS << " unique";
    OS << ")";
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
    E->getCastTypeLoc().getType().print(OS);
    OS << '\n';
    printRec(E->getSubExpr());
    OS << ')';
  }
  void visitConditionalCheckedCastExpr(ConditionalCheckedCastExpr *E) {
    printExplicitCastExpr(E, "conditional_checked_cast_expr");
  }
  void visitIsaExpr(IsaExpr *E) {
    printExplicitCastExpr(E, "is_subtype_expr");
  }
  void visitRebindSelfInConstructorExpr(RebindSelfInConstructorExpr *E) {
    printCommon(E, "rebind_self_in_constructor_expr") << '\n';
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
  void visitBindOptionalExpr(BindOptionalExpr *E) {
    OS.indent(Indent) << "(bind_optional_expr\n";
    printRec(E->getSubExpr());
    OS << ')';
  }
  void visitOptionalEvaluationExpr(OptionalEvaluationExpr *E) {
    OS.indent(Indent) << "(optional_evaluation_expr\n";
    printRec(E->getSubExpr());
    OS << ')';
  }
  void visitForceValueExpr(ForceValueExpr *E) {
    OS.indent(Indent) << "(force_value_expr\n";
    printRec(E->getSubExpr());
    OS << ')';
  }
};

} // end anonymous namespace.


void Expr::dump(raw_ostream &OS) const {
  print(OS);
  OS << '\n';
}

void Expr::dump() const {
  dump(llvm::errs());
}

void Expr::print(raw_ostream &OS, unsigned Indent) const {
  PrintExpr(OS, Indent).visit(const_cast<Expr*>(this));
}

//===----------------------------------------------------------------------===//
// Printing for TypeRepr and all subclasses.
//===----------------------------------------------------------------------===//

namespace {
class PrintTypeRepr : public TypeReprVisitor<PrintTypeRepr> {
public:
  raw_ostream &OS;
  unsigned Indent;
  bool ShowColors;

  PrintTypeRepr(raw_ostream &os, unsigned indent)
    : OS(os), Indent(indent), ShowColors(false) {
    if (&os == &llvm::errs() || &os == &llvm::outs())
      ShowColors = llvm::errs().has_colors() && llvm::outs().has_colors();
  }

  void printRec(Decl *D) { D->dump(OS, Indent + 2); }
  void printRec(Expr *E) { E->print(OS, Indent + 2); }
  void printRec(TypeRepr *T) { PrintTypeRepr(OS, Indent + 2).visit(T); }

  raw_ostream &printCommon(TypeRepr *T, const char *Name) {
    OS.indent(Indent) << '(';

    // Support optional color output.
    if (ShowColors) {
      if (const char *CStr =
          llvm::sys::Process::OutputColor(TypeReprColor, false, false)) {
        OS << CStr;
      }
    }

    OS << Name;

    if (ShowColors)
      OS << llvm::sys::Process::ResetColor();
    return OS;
  }

  void visitErrorTypeRepr(ErrorTypeRepr *T) {
    printCommon(T, "type_error");
  }

  void visitAttributedTypeRepr(AttributedTypeRepr *T) {
    printCommon(T, "type_attributed") << " attrs=";
    T->printAttrs(OS);
    OS << '\n';
    printRec(T->getTypeRepr());
  }

  void visitIdentTypeRepr(IdentTypeRepr *T) {
    printCommon(T, "type_ident");
    Indent += 2;
    for (auto &comp : T->Components) {
      OS << '\n';
      printCommon(nullptr, "component");
      OS << " id='" << comp.getIdentifier() << '\'';
      OS << " bind=";
      if (comp.isBoundDecl()) OS << "decl";
      else if (comp.isBoundModule()) OS << "module";
      else if (comp.isBoundType()) OS << "type";
      else OS << "none";
      OS << ')';
      for (auto genArg : comp.getGenericArgs()) {
        OS << '\n';
        printRec(genArg);
      }
    }
    OS << ')';
    Indent -= 2;
  }

  void visitFunctionTypeRepr(FunctionTypeRepr *T) {
    printCommon(T, "type_function");
    OS << '\n'; printRec(T->getArgsTypeRepr());
    OS << '\n'; printRec(T->getResultTypeRepr());
    OS << ')';
  }

  void visitArrayTypeRepr(ArrayTypeRepr *T) {
    printCommon(T, "type_array") << '\n';
    printRec(T->getBase());
    if (T->getSize()) {
      OS << '\n';
      printRec(T->getSize()->getExpr());
    }
    OS << ')';
  }

  void visitTupleTypeRepr(TupleTypeRepr *T) {
    printCommon(T, "type_tuple");
    for (auto elem : T->getElements()) {
      OS << '\n';
      printRec(elem);
    }
    OS << ')';
  }

  void visitNamedTypeRepr(NamedTypeRepr *T) {
    printCommon(T, "type_named");
    if (!T->getName().empty())
      OS << " id='" << T->getName();
    if (T->getTypeRepr()) {
      OS << '\n';
      printRec(T->getTypeRepr());
    }
    OS << ')';
  }

  void visitProtocolCompositionTypeRepr(ProtocolCompositionTypeRepr *T) {
    printCommon(T, "type_composite");
    for (auto elem : T->getProtocols()) {
      OS << '\n';
      printRec(elem);
    }
    OS << ')';
  }

  void visitMetaTypeTypeRepr(MetaTypeTypeRepr *T) {
    printCommon(T, "type_metatype") << '\n';
    printRec(T->getBase());
    OS << ')';
  }
};

} // end anonymous namespace.

void PrintDecl::printRec(TypeRepr *T) {
  PrintTypeRepr(OS, Indent+2).visit(T);
}

void PrintExpr::printRec(TypeRepr *T) {
  PrintTypeRepr(OS, Indent+2).visit(T);
}

void PrintPattern::printRec(TypeRepr *T) {
  PrintTypeRepr(OS, Indent+2).visit(T);
}

void TypeRepr::dump() const {
  PrintTypeRepr(llvm::errs(), 0).visit(const_cast<TypeRepr*>(this));
  llvm::errs() << '\n';
}

void Substitution::print(llvm::raw_ostream &os) const {
  Archetype->print(os);
  os << " = ";
  Replacement->print(os);
}

void Substitution::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}
