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

#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/PrintOptions.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Interleave.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

namespace {
  /// \brief AST pretty-printer.
  class PrintAST : public ASTVisitor<PrintAST> {
    raw_ostream &OS;
    const PrintOptions &Options;
    llvm::SmallVectorImpl<std::pair<Decl *, uint64_t>> *DeclOffsets;
    unsigned IndentLevel = 0;

    friend DeclVisitor<PrintAST>;

    /// \brief RAII object that increases the indentation level.
    class IndentRAII {
      PrintAST &Self;

    public:
      IndentRAII(PrintAST &self) : Self(self) {
        Self.IndentLevel += Self.Options.Indent;
      }

      ~IndentRAII() {
        Self.IndentLevel -= Self.Options.Indent;
      }
    };

    /// \brief Indent the current number of indentation spaces.
    void indent() {
      OS.indent(IndentLevel);
    }

    /// \brief Record the location of this declaration, which is about to
    /// be printed.
    void recordDeclLoc(Decl *decl) {
      if (DeclOffsets)
        DeclOffsets->push_back({decl, OS.tell()});
    }

    void printAttributes(const DeclAttributes &attrs);
    void printPattern(Pattern *pattern);
    void printGenericParams(GenericParamList *params);
    void printMembers(ArrayRef<Decl *> members, bool needComma = false);
    void printNominalDeclName(NominalTypeDecl *decl);
    void printInherited(ArrayRef<TypeLoc> inherited);
    void printBraceStmtElements(BraceStmt *stmt);

#define DECL(Name,Parent) void visit##Name##Decl(Name##Decl *decl);
#define ABSTRACT_DECL(Name, Parent)
#define DECL_RANGE(Name,Start,End)
#include "swift/AST/DeclNodes.def"

#define STMT(Name, Parent) void visit##Name##Stmt(Name##Stmt *stmt);
#include "swift/AST/StmtNodes.def"
    
  public:
    PrintAST(raw_ostream &os, const PrintOptions &options,
             llvm::SmallVectorImpl<std::pair<Decl *, uint64_t>> *declOffsets)
      : OS(os), Options(options), DeclOffsets(declOffsets) { }
  };
}

void PrintAST::printAttributes(const DeclAttributes &attrs) {
  if (attrs.empty())
    return;

  bool first = true;
  auto comma = [&]() {
    if (first) {
      first = false;
    } else {
      OS << ", ";
    }
  };

  OS << "[";
  if (attrs.isAssignment()) {
    comma();
    OS << "assignment";
  }
  if (attrs.isConversion()) {
    comma();
    OS << "conversion";
  }
  if (attrs.isForceInline()) {
    comma();
    OS << "force_inline";
  }
  if (attrs.isInfix()) {
    comma();
    OS << "infix";
  }
  if (attrs.getResilienceData().isValid()) {
    comma();
    switch (attrs.getResilienceData().getResilience()) {
    case Resilience::Fragile:
      OS << "fragile";
      break;

    case Resilience::InherentlyFragile:
      OS << "born_fragile";
      break;

    case Resilience::Resilient:
      OS << "resilient";
      break;
    }
  }
  if (attrs.isByref()) {
    comma();
    OS << "byref";
  }
  if (attrs.isAutoClosure()) {
    comma();
    OS << "auto_closure";
  }
  if (!attrs.AsmName.empty()) {
    comma();
    OS << "asmname=\"" << attrs.AsmName << "\"";
  }
  if (attrs.isPostfix()) {
    comma();
    OS << "postfix";
  }
  if (attrs.isObjC()) {
    comma();
    OS << "objc";
  }
  if (attrs.isObjCBlock()) {
    comma();
    OS << "objc_block";
  }
  if (attrs.isIBOutlet()) {
    comma();
    OS << "iboutlet";
  }
  if (attrs.isIBAction()) {
    comma();
    OS << "ibaction";
  }
  if (attrs.isClassProtocol()) {
    comma();
    OS << "class_protocol";
  }
  OS << "] ";
}

void PrintAST::printPattern(Pattern *pattern) {
  switch (pattern->getKind()) {
  case PatternKind::Any:
    OS << "_";
    break;

  case PatternKind::Named: {
    auto named = cast<NamedPattern>(pattern);
    recordDeclLoc(named->getDecl());
    OS << named->getBoundName();
    break;
  }

  case PatternKind::Paren:
    OS << "(";
    printPattern(cast<ParenPattern>(pattern)->getSubPattern());
    OS << ")";
    break;

  case PatternKind::Tuple: {
    OS << "(";
    auto tuple = cast<TuplePattern>(pattern);
    bool first = true;
    for (const auto &elt : tuple->getFields()) {
      if (first) {
        first = false;
      } else {
        OS << ", ";
      }
      printPattern(elt.getPattern());
      if (elt.isVararg()) {
        OS << "...";
      }
      if (Options.VarInitializers && elt.getInit()) {
        // FIXME: Print initializer here.
      }
    }
    OS << ")";
    break;
  }
      
  case PatternKind::Typed:
    auto typed = cast<TypedPattern>(pattern);
    printPattern(typed->getSubPattern());
    OS << " : ";
    if (typed->hasType())
      typed->getType()->print(OS);
    else
      OS << "<<null type>>";
    break;
  }
}

void PrintAST::printGenericParams(GenericParamList *params) {
  if (!params)
    return;

  OS << "<";
  bool first = true;
  for (auto gp : params->getParams()) {
    if (first) {
      first = false;
    } else {
      OS << ", ";
    }

    auto typeParam = gp.getAsTypeParam();
    OS << typeParam->getName().str();
    printInherited(typeParam->getInherited());
  }
  OS << ">";
}

void PrintAST::printMembers(ArrayRef<Decl *> members, bool needComma) {
  OS << " {\n";
  {
    IndentRAII indentMore(*this);
    for (auto member : members) {
      if (!member->shouldPrintInContext())
        continue;

      indent();
      visit(member);
      if (needComma && member != members.back())
        OS << ",";
      OS << "\n";
    }
  }
  indent();
  OS << "}";
}

void PrintAST::printNominalDeclName(NominalTypeDecl *decl) {
  OS << decl->getName();
  if (auto gp = decl->getGenericParams()) {
    printGenericParams(gp);
  }
}

void PrintAST::printInherited(ArrayRef<TypeLoc> inherited) {
  if (inherited.empty())
    return;

  OS << " : ";
  bool first = true;
  for (auto tl : inherited) {
    if (first) {
      first = false;
    } else {
      OS << ", ";
    }

    tl.getType()->print(OS);
  }
}

void PrintAST::visitImportDecl(ImportDecl *decl) {
  OS << "import ";
  recordDeclLoc(decl);
  bool first = true;
  for (auto elt : decl->getAccessPath()) {
    if (first) {
      first = false;
    } else {
      OS << '.';
    }

    OS << elt.first.str();
  }
  OS << "";
}

void PrintAST::visitExtensionDecl(ExtensionDecl *decl) {
  OS << "extension ";
  recordDeclLoc(decl);
  decl->getExtendedType().print(OS);
  printInherited(decl->getInherited());
  if (Options.TypeDefinitions) {
    printMembers(decl->getMembers());
  }
}

void PrintAST::visitPatternBindingDecl(PatternBindingDecl *decl) {
  recordDeclLoc(decl);
  OS << "var ";
  printPattern(decl->getPattern());
  if (Options.VarInitializers) {
    // FIXME: Implement once we can pretty-print expressions.
  }
}

void PrintAST::visitTopLevelCodeDecl(TopLevelCodeDecl *decl) {
  visit(decl->getBody());
}

void PrintAST::visitTypeAliasDecl(TypeAliasDecl *decl) {
  OS << "typealias ";
  printAttributes(decl->getAttrs());
  recordDeclLoc(decl);
  OS << decl->getName().str();
  printInherited(decl->getInherited());
  if (decl->hasUnderlyingType()) {
    OS << " = ";
    decl->getUnderlyingType().print(OS);
  }
}

void PrintAST::visitOneOfDecl(OneOfDecl *decl) {
  OS << "oneof ";
  printAttributes(decl->getAttrs());
  recordDeclLoc(decl);
  printNominalDeclName(decl);
  printInherited(decl->getInherited());
  if (Options.TypeDefinitions) {
    printMembers(decl->getMembers(), /*needComma=*/true);
  }
}

void PrintAST::visitStructDecl(StructDecl *decl) {
  OS << "struct ";
  printAttributes(decl->getAttrs());
  recordDeclLoc(decl);
  printNominalDeclName(decl);
  printInherited(decl->getInherited());
  if (Options.TypeDefinitions) {
    printMembers(decl->getMembers());
  }
}

void PrintAST::visitClassDecl(ClassDecl *decl) {
  OS << "class ";
  printAttributes(decl->getAttrs());
  recordDeclLoc(decl);
  printNominalDeclName(decl);
  printInherited(decl->getInherited());
  if (Options.TypeDefinitions) {
    printMembers(decl->getMembers());
  }
}

void PrintAST::visitProtocolDecl(ProtocolDecl *decl) {
  OS << "protocol ";
  printAttributes(decl->getAttrs());
  recordDeclLoc(decl);
  printNominalDeclName(decl);
  printInherited(decl->getInherited());
  if (Options.TypeDefinitions) {
    printMembers(decl->getMembers());
  }
}

void PrintAST::visitVarDecl(VarDecl *decl) {
  OS << "var ";
  printAttributes(decl->getAttrs());
  recordDeclLoc(decl);
  OS << decl->getName().str();
  if (decl->hasType()) {
    OS << " : ";
    decl->getType().print(OS);   
  }
  if (decl->isProperty()) {
    OS << " {";
    {
      if (auto getter = decl->getGetter()) {
        OS << "\n";
        indent();
        visit(getter);
        OS << "\n";
      }
      if (auto setter = decl->getSetter()) {
        OS << "\n";
        indent();
        visit(setter);
        OS << "\n";
      }
    }
    indent();
    OS << "}";
  }
}

void PrintAST::printBraceStmtElements(BraceStmt *stmt) {
  IndentRAII indentMore(*this);
  for (auto element : stmt->getElements()) {
    OS << "\n";
    indent();
    if (auto decl = element.dyn_cast<Decl*>()) {
      visit(decl);
    } else if (auto stmt = element.dyn_cast<Stmt*>()) {
      visit(stmt);
    } else {
      // FIXME: print expression
      // visit(element.get<Expr*>());
    }
  }
}


void PrintAST::visitFuncDecl(FuncDecl *decl) {
  if (decl->isGetterOrSetter()) {
    // FIXME: Attributes
    recordDeclLoc(decl);
    if (decl->getGetterDecl()) {
      OS << "get: ";
    } else {
      OS << "set";

      auto bodyParams = decl->getBody()->getBodyParamPatterns();
      auto valueParam = bodyParams.back()->getSemanticsProvidingPattern();
      if (auto named = dyn_cast<NamedPattern>(valueParam)) {
        OS << "(" << named->getBoundName().str() << ")";
      }
      OS << ": ";
    }

    if (!Options.FunctionDefinitions || !decl->getBody()->getBody()) {
      return;
    }
    
    printBraceStmtElements(decl->getBody()->getBody());
  } else {
    if (decl->isStatic() && !decl->isOperator())
      OS << "static ";
    OS << "func ";
    printAttributes(decl->getAttrs());
    recordDeclLoc(decl);
    if (decl->getName().empty())
      OS << "<anonymous>";
    else
      OS << decl->getName().str();
    if (decl->isGeneric()) {
      printGenericParams(decl->getGenericParams());
    }

    // FIXME: If arg/body parameter patterns differ, switch to
    // printing selector-style.
    bool first = true;
    for (auto pattern : decl->getBody()->getArgParamPatterns()) {
      if (first) {
        first = false;

        // Don't print the implicit 'this' parameter.
        if (decl->getDeclContext()->isTypeContext())
          continue;
      }

      printPattern(pattern);
    }

    auto &context = decl->getASTContext();
    Type resultTy = decl->getBody()->getResultType(context);
    if (resultTy && !resultTy->isEqual(TupleType::getEmpty(context))) {
      OS << " -> ";
      resultTy->print(OS);
    }
    
    if (!Options.FunctionDefinitions || !decl->getBody()->getBody()) {
      return;
    }
    
    OS << " ";
    visit(decl->getBody()->getBody());
  }
}

void PrintAST::visitOneOfElementDecl(OneOfElementDecl *decl) {
  // FIXME: Attributes?
  recordDeclLoc(decl);
  OS << decl->getName();

  if (decl->hasArgumentType()) {
    OS << " : ";
    decl->getArgumentType()->print(OS);
  }
}

void PrintAST::visitSubscriptDecl(SubscriptDecl *decl) {
  recordDeclLoc(decl);
  OS << "subscript ";
  printAttributes(decl->getAttrs());
  printPattern(decl->getIndices());
  OS << " -> ";
  decl->getElementType()->print(OS);
  OS << " {";
  {
    IndentRAII indentMore(*this);
    if (auto getter = decl->getGetter()) {
      OS << "\n";
      indent();
      visit(getter);
      OS << "\n";
    }
    if (auto setter = decl->getSetter()) {
      OS << "\n";
      indent();
      visit(setter);
      OS << "\n";
    }
  }
  indent();
  OS << "}";
}

void PrintAST::visitConstructorDecl(ConstructorDecl *decl) {
  recordDeclLoc(decl);
  OS << "constructor";
  if (decl->isGeneric()) {
    printGenericParams(decl->getGenericParams());
  } else {
    OS << " ";
  }
  printAttributes(decl->getAttrs());
  printPattern(decl->getArguments());
  if (!Options.FunctionDefinitions || !decl->getBody()) {
    return;
  }

  OS << " ";
  visit(decl->getBody());
}

void PrintAST::visitDestructorDecl(DestructorDecl *decl) {
  recordDeclLoc(decl);
  OS << "destructor ";
  printAttributes(decl->getAttrs());

  if (!Options.FunctionDefinitions || !decl->getBody()) {
    return;
  }

  OS << " ";
  visit(decl->getBody());
}

void PrintAST::visitInfixOperatorDecl(InfixOperatorDecl *decl) {
  recordDeclLoc(decl);
  OS << "operator infix " << decl->getName() << " {\n";
  {
    IndentRAII indentMore(*this);
    if (decl->getAssociativityLoc().isValid()) {
      indent();
      OS << "associativity ";
      switch (decl->getInfixData().getAssociativity()) {
      case Associativity::None:
        OS << "none\n";
        break;
      case Associativity::Left:
        OS << "left\n";
        break;
      case Associativity::Right:
        OS << "right\n";
        break;
      }
    }
    if (decl->getPrecedenceLoc().isValid()) {
      indent();
      OS << "precedence " << decl->getInfixData().getPrecedence() << '\n';
    }
  }
  indent();
  OS << "}";
}

void PrintAST::visitPrefixOperatorDecl(PrefixOperatorDecl *decl) {
  recordDeclLoc(decl);
  OS << "operator prefix " << decl->getName() << " {\n}";
}

void PrintAST::visitPostfixOperatorDecl(PostfixOperatorDecl *decl) {
  recordDeclLoc(decl);
  OS << "operator postfix " << decl->getName() << " {\n}";
}

void PrintAST::visitBraceStmt(BraceStmt *stmt) {
  OS << "{";
  printBraceStmtElements(stmt);
  OS << "\n";
  indent();
  OS << "}";
}

void PrintAST::visitReturnStmt(ReturnStmt *stmt) {
  OS << "return";
  if (stmt->hasResult()) {
    OS << " ";
    // FIXME: print expression.
  }
}

void PrintAST::visitIfStmt(IfStmt *stmt) {
  OS << "if ";
  // FIXME: print condition
  OS << " ";
  visit(stmt->getThenStmt());
  if (auto elseStmt = stmt->getElseStmt()) {
    OS << " else ";
    visit(elseStmt);
  }
}

void PrintAST::visitWhileStmt(WhileStmt *stmt) {
  OS << "while ";
  // FIXME: print condition
  OS << " ";
  visit(stmt->getBody());
}

void PrintAST::visitDoWhileStmt(DoWhileStmt *stmt) {
  OS << "do ";
  visit(stmt->getBody());
  OS << " while ";
  // FIXME: print condition
}

void PrintAST::visitForStmt(ForStmt *stmt) {
  OS << "for (";
  // FIXME: print initializer
  OS << "; ";
  if (stmt->getCond().isNonNull()) {
    // FIXME: print cond
  }
  OS << "; ";
  // FIXME: print increment
  OS << ") ";
  visit(stmt->getBody());
}

void PrintAST::visitForEachStmt(ForEachStmt *stmt) {
  OS << "for ";
  printPattern(stmt->getPattern());
  OS << " in ";
  // FIXME: print container
  OS << " ";
  visit(stmt->getBody());
}

void PrintAST::visitBreakStmt(BreakStmt *stmt) {
  OS << "break";
}

void PrintAST::visitContinueStmt(ContinueStmt *stmt) {
  OS << "continue";
}

void PrintAST::visitFallthroughStmt(FallthroughStmt *stmt) {
  OS << "fallthrough";
}

void PrintAST::visitSwitchStmt(SwitchStmt *stmt) {
  OS << "switch ";
  // FIXME: print subject
  OS << "{\n";
  for (CaseStmt *C : stmt->getCases()) {
    visit(C);
  }
  OS << '\n';
  indent();
  OS << "}";
}

void PrintAST::visitCaseStmt(CaseStmt *stmt) {
  if (stmt->isDefault())
    OS << "default:";
  else {
    OS << "case ";
    // FIXME: print case exprs
    OS << ':';
    printBraceStmtElements(cast<BraceStmt>(stmt->getBody()));
  }
}

void Decl::print(raw_ostream &os) const {
  PrintOptions options;
  options.FunctionDefinitions = true;
  options.TypeDefinitions = true;
  options.VarInitializers = true;

  PrintAST printer(os, options, nullptr);
  printer.visit(const_cast<Decl *>(this));
}

void Decl::print(
       raw_ostream &os, const PrintOptions &options,
       llvm::SmallVectorImpl<std::pair<Decl *, uint64_t>> *declOffsets) const {
  PrintAST printer(os, options, declOffsets);
  printer.visit(const_cast<Decl *>(this));
}

bool Decl::shouldPrintInContext() const {
  // Skip getters/setters. They are part of the property or subscript.
  if (isa<FuncDecl>(this) && cast<FuncDecl>(this)->isGetterOrSetter())
    return false;

  // Skip non-property variables, unless they came from a Clang module.
  // Non-property variables in Swift source will be picked up by the
  // PatternBindingDecl.
  if (isa<VarDecl>(this) && !this->hasClangNode() &&
      !cast<VarDecl>(this)->isProperty())
    return false;

  // Skip pattern bindings that consist of just one property.
  if (auto pbd = dyn_cast<PatternBindingDecl>(this)) {
    auto pattern = pbd->getPattern()->getSemanticsProvidingPattern();
    if (auto named = dyn_cast<NamedPattern>(pattern)) {
      if (named->getDecl()->isProperty()) {
        return false;
      }
    }
  }

  // FIXME: Skip implicitly-generated constructors.

  // Print everything else.
  return true;
}

