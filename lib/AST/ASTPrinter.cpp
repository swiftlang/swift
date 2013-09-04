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
#include "swift/Basic/STLExtras.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

namespace {
  /// \brief AST pretty-printer.
  class PrintAST : public ASTVisitor<PrintAST> {
    raw_ostream &OS;
    const PrintOptions &Options;
    SmallVectorImpl<std::pair<Decl *, uint64_t>> *DeclOffsets;
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
    void printPattern(const Pattern *pattern);
    void printGenericParams(GenericParamList *params);
    void printMembers(ArrayRef<Decl *> members, bool needComma = false);
    void printNominalDeclName(NominalTypeDecl *decl);
    void printInherited(ArrayRef<TypeLoc> inherited,
                        ArrayRef<ProtocolDecl *> protos,
                        Type superclass = {});

    template <typename DeclWithSuperclass>
    void printInheritedWithSuperclass(DeclWithSuperclass *decl);
    
    void printInherited(const TypeDecl *decl);
    void printInherited(const ExtensionDecl *decl);
    void printBraceStmtElements(BraceStmt *stmt);

    /// Print the argument/body patterns in selector style, if possible.
    ///
    /// \returns true if the arguments were printed in a selector style,
    /// false otherwise.
    bool printSelectorStyleArgs(ValueDecl *decl,
                                ArrayRef<Pattern *> argPatterns,
                                ArrayRef<Pattern *> bodyPatterns);

#define DECL(Name,Parent) void visit##Name##Decl(Name##Decl *decl);
#define ABSTRACT_DECL(Name, Parent)
#define DECL_RANGE(Name,Start,End)
#include "swift/AST/DeclNodes.def"

#define STMT(Name, Parent) void visit##Name##Stmt(Name##Stmt *stmt);
#include "swift/AST/StmtNodes.def"
    
  public:
    PrintAST(raw_ostream &os, const PrintOptions &options,
             SmallVectorImpl<std::pair<Decl *, uint64_t>> *declOffsets)
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
  if (attrs.isTransparent()) {
    comma();
    OS << "transparent";
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
  if (attrs.isThin()) {
    comma();
    OS << "thin";
  }
  if (attrs.isNoReturn()) {
    comma();
    OS << "noreturn";
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
  if (attrs.isExported()) {
    comma();
    OS << "exported";
  }
  OS << "] ";
}

void PrintAST::printPattern(const Pattern *pattern) {
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
      if (Options.VarInitializers && elt.getInit()) {
        // FIXME: Print initializer here.
      }
    }
    if (tuple->hasVararg()) {
      OS << "...";
    }
    OS << ")";
    break;
  }
      
  case PatternKind::Typed: {
    auto typed = cast<TypedPattern>(pattern);
    printPattern(typed->getSubPattern());
    OS << " : ";
    TypeLoc ty = typed->getTypeLoc();
    if (!ty.isNull()) {
      if (ty.getTypeRepr())
        OS << ty.getTypeRepr();
      else
        OS << ty.getType();
    } else {
      OS << "<<null type>>";
    }
    break;
  }
      
  case PatternKind::Isa: {
    auto isa = cast<IsaPattern>(pattern);
    OS << "is ";
    isa->getCastTypeLoc().getType().print(OS);
    break;
  }
      
  case PatternKind::NominalType: {
    auto type = cast<NominalTypePattern>(pattern);
    OS << type->getCastTypeLoc().getType();
    printPattern(type->getSubPattern());
    break;
  }
      
  case PatternKind::UnionElement: {
    auto elt = cast<UnionElementPattern>(pattern);
    // FIXME: Print element expr.
    if (elt->hasSubPattern())
      printPattern(elt->getSubPattern());
    break;
  }
      
  case PatternKind::Expr: {
    // FIXME: Print expr.
    break;
  }
      
  case PatternKind::Var: {
    auto var = cast<NominalTypePattern>(pattern);
    OS << "var ";
    printPattern(var->getSubPattern());
  }
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
    printInheritedWithSuperclass(typeParam);
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

void PrintAST::printInherited(ArrayRef<TypeLoc> inherited,
                              ArrayRef<ProtocolDecl *> protos,
                              Type superclass) {
  if (inherited.empty() && protos.empty() && superclass.isNull())
    return;

  OS << " : ";
  
  if (inherited.empty()) {
    if (superclass) {
      superclass.print(OS);
      OS << ", ";
    }
    
    interleave(protos, [&](const ProtocolDecl *proto) {
      proto->getDeclaredType()->print(OS);
    }, [&]() {
      OS << ", ";
    });
  } else {
    interleave(inherited, [&](TypeLoc TL) {
      TL.getType()->print(OS);
    }, [&]() {
      OS << ", ";
    });
  }
}

template <typename DeclWithSuperclass>
void PrintAST::printInheritedWithSuperclass(DeclWithSuperclass *decl) {
  printInherited(decl->getInherited(), decl->getProtocols(),
                 decl->getSuperclass());
}

void PrintAST::printInherited(const TypeDecl *decl) {
  printInherited(decl->getInherited(), decl->getProtocols());
}
void PrintAST::printInherited(const ExtensionDecl *decl) {
  printInherited(decl->getInherited(), decl->getProtocols());
}


void PrintAST::visitImportDecl(ImportDecl *decl) {
  OS << "import ";

  if (decl->isExported())
    OS << "[exported] ";

  switch (decl->getImportKind()) {
  case ImportKind::Module:
    break;
  case ImportKind::Type:
    OS << "typealias ";
    break;
  case ImportKind::Struct:
    OS << "struct ";
    break;
  case ImportKind::Class:
    OS << "class ";
    break;
  case ImportKind::Union:
    OS << "union ";
    break;
  case ImportKind::Protocol:
    OS << "protocol ";
    break;
  case ImportKind::Var:
    OS << "var ";
    break;
  case ImportKind::Func:
    OS << "func ";
    break;
  }
  recordDeclLoc(decl);
  interleave(decl->getFullAccessPath(),
             [&](const ImportDecl::AccessPathElement &Elem) {
               OS << Elem.first;
             },
             [&] { OS << '.'; });
}

void PrintAST::visitExtensionDecl(ExtensionDecl *decl) {
  OS << "extension ";
  recordDeclLoc(decl);
  decl->getExtendedType().print(OS);
  printInherited(decl);
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
  printInherited(decl);
  if (Options.TypeDefinitions && decl->hasUnderlyingType()) {
    OS << " = ";
    decl->getUnderlyingType().print(OS);
  }
}

void PrintAST::visitGenericTypeParamDecl(GenericTypeParamDecl *decl) {
  recordDeclLoc(decl);
  OS << decl->getName().str();
  printInheritedWithSuperclass(decl);
}

void PrintAST::visitAssociatedTypeDecl(AssociatedTypeDecl *decl) {
  OS << "typealias ";
  printAttributes(decl->getAttrs());
  recordDeclLoc(decl);
  OS << decl->getName().str();
  printInheritedWithSuperclass(decl);
}

void PrintAST::visitUnionDecl(UnionDecl *decl) {
  OS << "union ";
  printAttributes(decl->getAttrs());
  recordDeclLoc(decl);
  printNominalDeclName(decl);
  printInherited(decl);
  if (Options.TypeDefinitions) {
    printMembers(decl->getMembers());
  }
}

void PrintAST::visitStructDecl(StructDecl *decl) {
  OS << "struct ";
  printAttributes(decl->getAttrs());
  recordDeclLoc(decl);
  printNominalDeclName(decl);
  printInherited(decl);
  if (Options.TypeDefinitions) {
    printMembers(decl->getMembers());
  }
}

void PrintAST::visitClassDecl(ClassDecl *decl) {
  OS << "class ";
  printAttributes(decl->getAttrs());
  recordDeclLoc(decl);
  printNominalDeclName(decl);
  printInheritedWithSuperclass(decl);
  if (Options.TypeDefinitions) {
    printMembers(decl->getMembers());
  }
}

void PrintAST::visitProtocolDecl(ProtocolDecl *decl) {
  OS << "protocol ";
  printAttributes(decl->getAttrs());
  recordDeclLoc(decl);
  printNominalDeclName(decl);
  printInherited(decl);
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

  if (decl->isProperty() && Options.FunctionDefinitions) {
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

bool PrintAST::printSelectorStyleArgs(ValueDecl *decl,
                                      ArrayRef<Pattern *> argPatterns,
                                      ArrayRef<Pattern *> bodyPatterns) {
  // Skip over the implicit 'self'.
  if (decl->getDeclContext()->isTypeContext()) {
    argPatterns = argPatterns.slice(1);
    bodyPatterns = bodyPatterns.slice(1);
  }

  // Only works for a single pattern.
  if (argPatterns.size() != 1 || bodyPatterns.size() != 1)
    return false;

  // Only applies when we have a tuple with at least two elements.
  auto argTuple = argPatterns[0]->getType()->getAs<TupleType>();
  auto bodyTuple = bodyPatterns[0]->getType()->getAs<TupleType>();
  if (!argTuple || argTuple->getFields().size() < 2 ||
      !bodyTuple ||
      bodyTuple->getFields().size() != argTuple->getFields().size())
    return false;

  // Step through the elements one by one, checking whether we have a name
  // mismatch at any point. That implies we need to use selector style.
  // FIXME: This is bogus. We should record whether selector style was used
  // within the AST.
  bool mismatched = false;
  for (unsigned i = 1, n = argTuple->getFields().size(); i != n; ++i) {
    if (argTuple->getFields()[i].getName() !=
          bodyTuple->getFields()[i].getName()) {
      mismatched = true;
      break;
    }
  }

  // All of the names match up. There's nothing to do here.
  if (!mismatched)
    return false;

  // Print in selector style.
  for (unsigned i = 0, n = argTuple->getFields().size(); i != n; ++i) {
    auto argName = argTuple->getFields()[i].getName();
    auto bodyName = bodyTuple->getFields()[i].getName();
    if (i != 0) {
      OS << ' ';
      if (argName.empty())
        OS << '_';
      else
        OS << argName.str();
    }

    OS << "(";
    if (bodyName.empty())
      OS << '_';
    else
      OS << bodyName.str();

    OS << ": ";
    argTuple->getElementType(i).print(OS);
    OS << ")";
  }
  return true;
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

    if (!printSelectorStyleArgs(decl,
                                decl->getBody()->getArgParamPatterns(),
                                decl->getBody()->getBodyParamPatterns())) {
      bool first = true;
      for (auto pattern : decl->getBody()->getArgParamPatterns()) {
        if (first) {
          first = false;

          // Don't print the implicit 'self' parameter.
          if (decl->getDeclContext()->isTypeContext())
            continue;
        }

        printPattern(pattern);
      }
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

void PrintAST::visitUnionElementDecl(UnionElementDecl *decl) {
  // FIXME: Attributes?
  recordDeclLoc(decl);
  OS << "case ";
  OS << decl->getName();

  if (decl->hasArgumentType())
    decl->getArgumentType().print(OS);
  
  if (decl->hasResultType()) {
    OS << " -> ";
    decl->getResultType().print(OS);
  }
}

void PrintAST::visitSubscriptDecl(SubscriptDecl *decl) {
  recordDeclLoc(decl);
  OS << "subscript ";
  printAttributes(decl->getAttrs());
  printPattern(decl->getIndices());
  OS << " -> ";
  decl->getElementType().print(OS);

  if (!Options.FunctionDefinitions)
    return;

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
      switch (decl->getAssociativity()) {
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
      OS << "precedence " << decl->getPrecedence() << '\n';
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
  auto printCaseLabel = [&](CaseLabel *label) {
    if (label->isDefault()) {
      OS << "default";
      // '_' pattern is implicit and doesn't need to be printed.
    } else {
      OS << "case ";
      interleave(label->getPatterns(),
                 [&](Pattern *p) { printPattern(p); },
                 [&] { OS << ", "; });
    }
    if (label->getGuardExpr()) {
      OS << " where ";
      // FIXME: print guard expr
    }
    OS << ":\n";
  };

  for (auto *label : stmt->getCaseLabels())
    printCaseLabel(label);

  printBraceStmtElements(cast<BraceStmt>(stmt->getBody()));
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
       SmallVectorImpl<std::pair<Decl *, uint64_t>> *declOffsets) const {
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

