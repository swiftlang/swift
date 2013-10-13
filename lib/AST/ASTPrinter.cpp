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

#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/PrintOptions.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/TypeVisitor.h"
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
      OS.indent(IndentLevel);
    }

    /// \brief Record the location of this declaration, which is about to
    /// be printed.
    void recordDeclLoc(Decl *decl) {
      if (DeclOffsets)
        DeclOffsets->push_back({decl, OS.tell()});
    }

    void printTypeLoc(const TypeLoc &TL) {
      // Print a TypeRepr if instructed to do so by options, or if the type
      // is null.
      if ((Options.PreferTypeRepr && TL.hasLocation()) ||
          TL.getType().isNull()) {
        TL.getTypeRepr()->print(OS);
        return;
      }
      TL.getType().print(OS, Options);
    }

    void printAttributes(const DeclAttributes &attrs);
    void printTypedPattern(const TypedPattern *TP,
                           bool StripOuterSliceType = false);
public:
    void printPattern(const Pattern *pattern);

    void printGenericParams(GenericParamList *params);

private:
    void printMembers(ArrayRef<Decl *> members, bool needComma = false);
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
    void printBraceStmtElements(BraceStmt *stmt, bool NeedIndent = true);

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
    PrintAST(raw_ostream &os, const PrintOptions &options,
             SmallVectorImpl<std::pair<Decl *, uint64_t>> *declOffsets)
      : OS(os), Options(options), DeclOffsets(declOffsets) { }
  };
} // unnamed namespace

void PrintAST::printAttributes(const DeclAttributes &Attrs) {
  if (Attrs.empty())
    return;

  if (Attrs.isAssignment())
    OS << "@assignment ";
  if (Attrs.isConversion())
    OS << "@conversion ";
  if (Attrs.isTransparent())
    OS << "@transparent ";
  if (Attrs.isInfix())
    OS << "@infix ";
  switch (Attrs.getResilienceKind()) {
  case Resilience::Default: break;
  case Resilience::Fragile: OS << "@fragile "; break;
  case Resilience::InherentlyFragile: OS << "@born_fragile "; break;
  case Resilience::Resilient: OS << "@resilient "; break;
  }
  if (Attrs.isNoReturn())
    OS << "@noreturn ";
  if (!Attrs.AsmName.empty())
    OS << "@asmname=\"" << Attrs.AsmName << "\" ";
  if (Attrs.isPostfix())
    OS << "@postfix ";
  if (Attrs.isObjC())
    OS << "@objc ";
  if (Attrs.isIBOutlet())
    OS << "@iboutlet ";
  if (Attrs.isIBAction())
    OS << "@ibaction ";
  if (Attrs.isClassProtocol())
    OS << "@class_protocol ";
  if (Attrs.isExported())
    OS << "@exported ";
}

void PrintAST::printTypedPattern(const TypedPattern *TP,
                                 bool StripOuterSliceType) {
  printPattern(TP->getSubPattern());
  OS << ": ";
  if (StripOuterSliceType) {
    Type T = TP->getType();
    if (auto *BGT = T->getAs<BoundGenericType>()) {
      BGT->getGenericArgs()[0].print(OS, Options);
      return;
    }
  }
  printTypeLoc(TP->getTypeLoc());
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
    auto TP = cast<TuplePattern>(pattern);
    auto Fields = TP->getFields();
    for (unsigned i = 0, e = Fields.size(); i != e; ++i) {
      const auto &Elt = Fields[i];
      if (i != 0)
        OS << ", ";

      if (i == e - 1 && TP->hasVararg()) {
        printTypedPattern(cast<TypedPattern>(Elt.getPattern()),
                          /*StripOuterSliceType=*/true);
      } else {
        printPattern(Elt.getPattern());
      }
      if (Options.VarInitializers && Elt.getInit()) {
        // FIXME: Print initializer here.
      }
    }
    if (TP->hasVararg())
      OS << "...";
    OS << ")";
    break;
  }

  case PatternKind::Typed:
    printTypedPattern(cast<TypedPattern>(pattern));
    break;

  case PatternKind::Isa: {
    auto isa = cast<IsaPattern>(pattern);
    OS << "is ";
    isa->getCastTypeLoc().getType().print(OS);
    break;
  }
      
  case PatternKind::NominalType: {
    auto type = cast<NominalTypePattern>(pattern);
    OS << type->getCastTypeLoc().getType() << '(';
    interleave(type->getElements().begin(), type->getElements().end(),
               [&](const NominalTypePattern::Element &elt) {
                 OS << elt.getPropertyName().str() << ':';
                 printPattern(elt.getSubPattern());
               }, [&] {
                 OS << ", ";
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
      
  case PatternKind::Expr: {
    // FIXME: Print expr.
    break;
  }
      
  case PatternKind::Var: {
    auto var = cast<VarPattern>(pattern);
    OS << "var ";
    printPattern(var->getSubPattern());
  }
  }
}

void PrintAST::printGenericParams(GenericParamList *Params) {
  if (!Params)
    return;

  OS << "<";
  bool IsFirst = true;
  for (auto GP : Params->getParams()) {
    if (IsFirst) {
      IsFirst = false;
    } else {
      OS << ", ";
    }

    auto TypeParam = GP.getAsTypeParam();
    OS << TypeParam->getName().str();
    printInherited(TypeParam);
  }

  auto Requirements = Params->getRequirements();
  if (!Requirements.empty()) {
    OS << " where";
    bool IsFirst = true;
    for (auto &Req : Requirements) {
      if (Req.isInvalid())
        continue;
      if (IsFirst) {
        OS << " ";
        IsFirst = false;
      } else {
        OS << ", ";
      }
      switch (Req.getKind()) {
      case RequirementKind::Conformance:
        printTypeLoc(Req.getSubjectLoc());
        OS << " : ";
        printTypeLoc(Req.getConstraintLoc());
        break;
      case RequirementKind::SameType:
        printTypeLoc(Req.getFirstTypeLoc());
        OS << " == ";
        printTypeLoc(Req.getSecondTypeLoc());
        break;
      }
    }
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

      if (Options.SkipImplicit && member->isImplicit())
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
    // If only conforms to DynamicLookup protocol, nothing to print.
    if (protos.size() == 1) {
      if (protos.front()->isSpecificProtocol(KnownProtocolKind::DynamicLookup))
        return;
    }
  }

  OS << " : ";

  if (inherited.empty()) {
    bool PrintedInherited = false;

    if (superclass) {
      superclass.print(OS);
      PrintedInherited = true;
    }

    bool UseProtocolCompositionSyntax =
        PrintAsProtocolComposition && protos.size() > 1;
    if (UseProtocolCompositionSyntax)
      OS << "protocol<";
    for (auto Proto : protos) {
      if (Proto->isSpecificProtocol(KnownProtocolKind::DynamicLookup))
        continue;
      if (isa<EnumDecl>(decl)
          && cast<EnumDecl>(decl)->hasRawType()
          && Proto->isSpecificProtocol(KnownProtocolKind::RawRepresentable))
        continue;
      
      if (PrintedInherited)
        OS << ", ";
      Proto->getDeclaredType()->print(OS);
      PrintedInherited = true;
    }
    if (UseProtocolCompositionSyntax)
      OS << ">";
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
  if (decl->isExported())
    OS << "@exported ";

  OS << "import ";

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
  case ImportKind::Enum:
    OS << "enum ";
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
  printBraceStmtElements(decl->getBody(), /*NeedIndent=*/false);
}

void PrintAST::visitTypeAliasDecl(TypeAliasDecl *decl) {
  printAttributes(decl->getAttrs());
  OS << "typealias ";
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
  printAttributes(decl->getAttrs());
  OS << "typealias ";
  recordDeclLoc(decl);
  OS << decl->getName().str();
  printInheritedWithSuperclass(decl);
}

void PrintAST::visitEnumDecl(EnumDecl *decl) {
  printAttributes(decl->getAttrs());
  OS << "enum ";
  recordDeclLoc(decl);
  printNominalDeclName(decl);
  printInherited(decl);
  if (Options.TypeDefinitions) {
    printMembers(decl->getMembers());
  }
}

void PrintAST::visitStructDecl(StructDecl *decl) {
  printAttributes(decl->getAttrs());
  OS << "struct ";
  recordDeclLoc(decl);
  printNominalDeclName(decl);
  printInherited(decl);
  if (Options.TypeDefinitions) {
    printMembers(decl->getMembers());
  }
}

void PrintAST::visitClassDecl(ClassDecl *decl) {
  printAttributes(decl->getAttrs());
  OS << "class ";
  recordDeclLoc(decl);
  printNominalDeclName(decl);
  printInheritedWithSuperclass(decl);
  if (Options.TypeDefinitions) {
    printMembers(decl->getMembers());
  }
}

void PrintAST::visitProtocolDecl(ProtocolDecl *decl) {
  printAttributes(decl->getAttrs());
  OS << "protocol ";
  recordDeclLoc(decl);
  printNominalDeclName(decl);
  printInherited(decl);
  if (Options.TypeDefinitions) {
    printMembers(decl->getMembers());
  }
}

void PrintAST::visitVarDecl(VarDecl *decl) {
  printAttributes(decl->getAttrs());
  OS << "var ";
  recordDeclLoc(decl);
  OS << decl->getName().str();
  if (decl->hasType()) {
    OS << ": ";
    decl->getType().print(OS);   
  }

  if (decl->isComputed() && Options.FunctionDefinitions) {
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

void PrintAST::printFunctionParameters(AbstractFunctionDecl *AFD) {
  ArrayRef<Pattern *> ArgPatterns = AFD->getArgParamPatterns();
  ArrayRef<Pattern *> BodyPatterns = AFD->getBodyParamPatterns();

  // Skip over the implicit 'self'.
  if (AFD->getImplicitSelfDecl() && isa<FuncDecl>(AFD)) {
    ArgPatterns = ArgPatterns.slice(1);
    BodyPatterns = BodyPatterns.slice(1);
  }

  if (!AFD->hasSelectorStyleSignature()) {
    for (auto Pat : ArgPatterns) {
      printPattern(Pat);
    }
    return;
  }

  auto ArgTuple = cast<TuplePattern>(ArgPatterns[0]);
  auto BodyTuple = cast<TuplePattern>(BodyPatterns[0]);

  // Print in selector style.
  for (unsigned i = 0, e = ArgTuple->getFields().size(); i != e; ++i) {
    if (isa<ConstructorDecl>(AFD) || (isa<FuncDecl>(AFD) && i != 0)) {
      OS << ' ';
      auto ArgName = ArgTuple->getFields()[i].getPattern()->getBoundName();
      if (ArgName.empty())
        OS << '_';
      else
        OS << ArgName.str();
    }

    OS << '(';
    printPattern(BodyTuple->getFields()[i].getPattern());
    OS << ')';
  }
}

void PrintAST::printBraceStmtElements(BraceStmt *stmt, bool NeedIndent) {
  IndentRAII IndentMore(*this, NeedIndent);
  for (auto element : stmt->getElements()) {
    OS << "\n";
    indent();
    if (auto decl = element.dyn_cast<Decl*>()) {
      if (decl->shouldPrintInContext())
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
      OS << "get:";
    } else {
      OS << "set";

      auto BodyParams = decl->getBodyParamPatterns();
      auto ValueParam = BodyParams.back()->getSemanticsProvidingPattern();
      if (auto *TP = dyn_cast<TuplePattern>(ValueParam)) {
        if (!TP->isImplicit()) {
          for (auto &Elt : TP->getFields()) {
            Identifier Name = Elt.getPattern()->getBoundName();
            if (!Name.empty())
              OS << "(" << Name.str() << ")";
          }
        }
      }
      OS << ":";
    }

    if (!Options.FunctionDefinitions || !decl->getBody()) {
      return;
    }
    
    printBraceStmtElements(decl->getBody());
  } else {
    printAttributes(decl->getAttrs());
    if (decl->isStatic() && !decl->isOperator())
      OS << "static ";
    OS << "func ";
    recordDeclLoc(decl);
    if (decl->getName().empty())
      OS << "<anonymous>";
    else
      OS << decl->getName().str();
    if (decl->isGeneric()) {
      printGenericParams(decl->getGenericParams());
    }

    printFunctionParameters(decl);

    auto &Context = decl->getASTContext();
    Type ResultTy = decl->getResultType(Context);
    if (ResultTy && !ResultTy->isEqual(TupleType::getEmpty(Context))) {
      OS << " -> ";
      ResultTy->print(OS);
    }
    
    if (!Options.FunctionDefinitions || !decl->getBody()) {
      return;
    }
    
    OS << " ";
    visit(decl->getBody());
  }
}

static void printEnumElement(raw_ostream &OS, EnumElementDecl *elt) {
  OS << elt->getName();
  
  if (elt->hasArgumentType())
    elt->getArgumentType().print(OS);
  
  if (elt->hasResultType()) {
    OS << " -> ";
    elt->getResultType().print(OS);
  }
}

void PrintAST::visitEnumCaseDecl(EnumCaseDecl *decl) {
  // FIXME: Attributes?
  recordDeclLoc(decl);
  OS << "case ";
  
  interleave(decl->getElements().begin(), decl->getElements().end(),
    [&](EnumElementDecl *elt) {
      printEnumElement(OS, elt);
    },
    [&] { OS << ", "; });
}

void PrintAST::visitEnumElementDecl(EnumElementDecl *decl) {
  if (!decl->shouldPrintInContext())
    return;

  // In cases where there is no parent EnumCaseDecl (such as imported or
  // deserialized elements), print the element independently.
  OS << "case ";
  printEnumElement(OS, decl);
}

void PrintAST::visitSubscriptDecl(SubscriptDecl *decl) {
  recordDeclLoc(decl);
  printAttributes(decl->getAttrs());
  OS << "subscript ";
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
  printAttributes(decl->getAttrs());
  OS << "init";
  if (decl->isGeneric()) {
    printGenericParams(decl->getGenericParams());
  }
  printFunctionParameters(decl);
  if (!Options.FunctionDefinitions || !decl->getBody()) {
    return;
  }

  OS << " ";
  visit(decl->getBody());
}

void PrintAST::visitDestructorDecl(DestructorDecl *decl) {
  recordDeclLoc(decl);
  printAttributes(decl->getAttrs());
  OS << "destructor() ";

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
  // Skip getters/setters. They are part of the variable or subscript.
  if (isa<FuncDecl>(this) && cast<FuncDecl>(this)->isGetterOrSetter())
    return false;

  // Skip stored variables, unless they came from a Clang module.
  // Stored variables in Swift source will be picked up by the
  // PatternBindingDecl.
  if (isa<VarDecl>(this) && !this->hasClangNode() &&
      !cast<VarDecl>(this)->isComputed())
    return false;

  // Skip pattern bindings that consist of just one computed variable.
  if (auto pbd = dyn_cast<PatternBindingDecl>(this)) {
    auto pattern = pbd->getPattern()->getSemanticsProvidingPattern();
    if (auto named = dyn_cast<NamedPattern>(pattern)) {
      if (named->getDecl()->isComputed()) {
        return false;
      }
    }
  }

  if (auto EED = dyn_cast<EnumElementDecl>(this)) {
    // Enum elements are printed as part of the EnumCaseDecl, unless they were
    // imported without source info.
    return !EED->getSourceRange().isValid();
  }

  // FIXME: Skip implicitly-generated constructors.

  // Print everything else.
  return true;
}

void Pattern::print(llvm::raw_ostream &OS, const PrintOptions &Options) const {
  PrintAST Printer(OS, Options, nullptr);
  Printer.printPattern(this);
}

//===----------------------------------------------------------------------===//
//  Type Printing
//===----------------------------------------------------------------------===//

namespace {
class TypePrinter : public TypeVisitor<TypePrinter> {
  raw_ostream &OS;
  const PrintOptions &Options;

  void printDeclContext(DeclContext *DC) {
    switch (DC->getContextKind()) {
    case DeclContextKind::Module: {
      Module *M = cast<Module>(DC);

      if (auto Parent = M->getParent())
        printDeclContext(Parent);
      OS << M->Name;
      return;
    }

    case DeclContextKind::AbstractClosureExpr:
      // FIXME: print closures somehow.
      return;

    case DeclContextKind::NominalTypeDecl:
      visit(cast<NominalTypeDecl>(DC)->getType());
      return;

    case DeclContextKind::ExtensionDecl:
      visit(cast<ExtensionDecl>(DC)->getExtendedType());
      return;

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

    OS << '<';
    bool First = true;
    for (Type Arg : Args) {
      if (First)
        First = false;
      else
        OS << ", ";
      visit(Arg);
    }
    OS << '>';
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

    switch (T->getKind()) {
    case TypeKind::Array:
    case TypeKind::ArraySlice:
    case TypeKind::Function:
    case TypeKind::PolymorphicFunction:
    case TypeKind::GenericFunction:
      OS << '(';
      visit(T);
      OS << ')';
      break;

    default:
      visit(T);
    }
  }

  void printGenericParams(GenericParamList *Params) {
    PrintAST(OS, Options, nullptr).printGenericParams(Params);
  }

public:
  TypePrinter(raw_ostream &OS, const PrintOptions &PO)
      : OS(OS), Options(PO) {}

  void visitErrorType(ErrorType *T) {
    OS << "<<error type>>";
  }

  void visitBuiltinRawPointerType(BuiltinRawPointerType *T) {
    OS << "Builtin.RawPointer";
  }

  void visitBuiltinObjectPointerType(BuiltinObjectPointerType *T) {
    OS << "Builtin.ObjectPointer";
  }

  void visitBuiltinObjCPointerType(BuiltinObjCPointerType *T) {
    OS << "Builtin.ObjCPointer";
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

    OS << "Builtin.Vec" << T->getNumElements() << "x" << UnderlyingStr;
  }

  void visitBuiltinIntegerType(BuiltinIntegerType *T) {
    OS << "Builtin.Int" << T->getBitWidth();
  }

  void visitBuiltinFloatType(BuiltinFloatType *T) {
    switch (T->getFPKind()) {
    case BuiltinFloatType::IEEE16:  OS << "Builtin.FPIEEE16"; return;
    case BuiltinFloatType::IEEE32:  OS << "Builtin.FPIEEE32"; return;
    case BuiltinFloatType::IEEE64:  OS << "Builtin.FPIEEE64"; return;
    case BuiltinFloatType::IEEE80:  OS << "Builtin.FPIEEE80"; return;
    case BuiltinFloatType::IEEE128: OS << "Builtin.FPIEEE128"; return;
    case BuiltinFloatType::PPC128:  OS << "Builtin.FPPPC128"; return;
    }
  }

  void visitNameAliasType(NameAliasType *T) {
    if (Options.FullyQualifiedTypes) {
      if (auto ParentDC = T->getDecl()->getDeclContext()) {
        printDeclContext(ParentDC);
        OS << '.';
      }
    }
    OS << T->getDecl()->getName().get();
  }

  void visitParenType(ParenType *T) {
    OS << '(';
    visit(T->getUnderlyingType());
    OS << ')';
  }

  void visitTupleType(TupleType *T) {
    OS << "(";

    auto Fields = T->getFields();
    for (unsigned i = 0, e = Fields.size(); i != e; ++i) {
      if (i)
        OS << ", ";
      const TupleTypeElt &TD = Fields[i];

      if (TD.hasName())
        OS << TD.getName() << ": ";

      if (TD.isVararg())
        OS <<  TD.getVarargBaseTy() << "...";
      else
        OS << TD.getType();
    }
    OS << ')';
  }

  void visitUnboundGenericType(UnboundGenericType *T) {
    if (auto ParentType = T->getParent()) {
      visit(ParentType);
      OS << ".";
    } else if (Options.FullyQualifiedTypes) {
      OS << T->getDecl()->getModuleContext()->Name << ".";
    }
    OS << T->getDecl()->getName().get();
  }

  void visitBoundGenericType(BoundGenericType *T) {
    if (Options.SynthesizeSugarOnTypes) {
      auto *NT = T->getAnyNominal();
      auto &Ctx = T->getASTContext();
      if (NT == Ctx.getSliceDecl()) {
        printWithParensIfNotSimple(T->getGenericArgs()[0]);
        OS << "[]";
        return;
      }
      if (NT == Ctx.getOptionalDecl()) {
        printWithParensIfNotSimple(T->getGenericArgs()[0]);
        OS << "?";
        return;
      }
    }
    if (auto ParentType = T->getParent()) {
      visit(ParentType);
      OS << ".";
    } else if (Options.FullyQualifiedTypes) {
      OS << T->getDecl()->getModuleContext()->Name << ".";
    }

    OS << T->getDecl()->getName().get();
    printGenericArgs(T->getGenericArgs());
  }

  void visitEnumType(EnumType *T) {
    if (auto ParentType = T->getParent()) {
      visit(ParentType);
      OS << ".";
    } else if (Options.FullyQualifiedTypes) {
      OS << T->getDecl()->getModuleContext()->Name << ".";
    }

    OS << T->getDecl()->getName().get();
  }

  void visitStructType(StructType *T) {
    if (auto ParentType = T->getParent()) {
      visit(ParentType);
      OS << ".";
    } else if (Options.FullyQualifiedTypes) {
      OS << T->getDecl()->getModuleContext()->Name << ".";
    }

    OS << T->getDecl()->getName().get();
  }

  void visitClassType(ClassType *T) {
    if (auto ParentType = T->getParent()) {
      visit(ParentType);
      OS << ".";
    } else if (Options.FullyQualifiedTypes) {
      OS << T->getDecl()->getModuleContext()->Name << ".";
    }

    OS << T->getDecl()->getName().get();
  }

  void visitMetaTypeType(MetaTypeType *T) {
    printWithParensIfNotSimple(T->getInstanceType());
    OS << ".metatype";
  }

  void visitModuleType(ModuleType *T) {
    OS << "module<" << T->getModule()->Name << '>';
  }

  void printFunctionExtInfo(AnyFunctionType::ExtInfo info) {
    if (info.isAutoClosure())
      OS << "@auto_closure ";
    switch (info.getCC()) {
    case AbstractCC::Freestanding: break;
    case AbstractCC::Method:
      OS << "@cc(method) ";
      break;
    case AbstractCC::C:
      OS << "@cc(cdecl) ";
      break;
    case AbstractCC::ObjCMethod:
      OS << "@cc(objc_method) ";
      break;
    }

    if (info.isBlock())
      OS << "@objc_block ";
    if (info.isThin())
      OS << "@thin ";
    if (info.isNoReturn())
      OS << "@noreturn ";
  }

  void visitFunctionType(FunctionType *T) {
    printFunctionExtInfo(T->getExtInfo());
    printWithParensIfNotSimple(T->getInput());
    OS << " -> " << T->getResult();
  }

  void visitPolymorphicFunctionType(PolymorphicFunctionType *T) {
    printFunctionExtInfo(T->getExtInfo());
    printGenericParams(&T->getGenericParams());
    OS << ' ';
    printWithParensIfNotSimple(T->getInput());

    OS << " -> " << T->getResult();
  }

  void visitGenericFunctionType(GenericFunctionType *T) {
    printFunctionExtInfo(T->getExtInfo());

    // Print the generic parameters.
    OS << '<';
    bool isFirstParam = true;
    for (auto param : T->getGenericParams()) {
      if (isFirstParam)
        isFirstParam = false;
      else
        OS << ", ";

      visit(param);
    }

    // Print the requirements.
    bool isFirstReq = true;
    for (const auto &req : T->getRequirements()) {
      if (isFirstReq) {
        OS << " where ";
        isFirstReq = false;
      } else {
        OS << ", ";
      }

      visit(req.getFirstType());
      switch (req.getKind()) {
      case RequirementKind::Conformance:
        OS << " : ";
        break;

      case RequirementKind::SameType:
        OS << " == ";
        break;
      }
      visit(req.getSecondType());
    }
    OS << '>';

    OS << ' ';
    printWithParensIfNotSimple(T->getInput());

    OS << " -> " << T->getResult();
  }

  void visitSILFunctionType(SILFunctionType *T) {
    OS << "func ";
    printFunctionExtInfo(T->getExtInfo());
    if (auto generics = T->getGenericParams())
      printGenericParams(generics);
    OS << '(';
    bool first = true;
    for (auto param : T->getParameters()) {
      if (first) {
        first = false;
      } else {
        OS << ", ";
      }
      param.print(OS, Options);
    }
    OS << ") -> ";

    T->getResult().print(OS, Options);
  }

  void visitArrayType(ArrayType *T) {
    printWithParensIfNotSimple(T->getBaseType());
    OS << '[' << T->getSize() << ']';
  }

  void visitArraySliceType(ArraySliceType *T) {
    printWithParensIfNotSimple(T->getBaseType());
    OS << "[]";
  }

  void visitOptionalType(OptionalType *T) {
    printWithParensIfNotSimple(T->getBaseType());
    OS << '?';
  }

  void visitVecType(VecType *T) {
    OS << "Vec<";
    visit(T->getBaseType());
    OS << ", " << T->getLength() << ">";
  }

  void visitMatrixType(MatrixType *T) {
    OS << "Matrix<";
    visit(T->getBaseType());
    OS << ", " << T->getRows();
    if (auto columns = T->getColumnsAsSpecified()) {
      OS << ", " << *columns;
    }
    OS << ">";
  }

  void visitProtocolType(ProtocolType *T) {
    OS << T->getDecl()->getName().str();
  }

  void visitProtocolCompositionType(ProtocolCompositionType *T) {
    OS << "protocol<";
    bool First = true;
    for (auto Proto : T->getProtocols()) {
      if (First)
        First = false;
      else
        OS << ", ";
      visit(Proto);
    }
    OS << ">";
  }

  void visitLValueType(LValueType *T) {
    OS << "@inout ";

    LValueType::Qual QS = T->getQualifiers();
    if (QS != LValueType::Qual::DefaultForType) {
      bool HasQual = false;
#define APPEND_QUAL(Cond, Text)        \
      do {                             \
        if (Cond) {                    \
          if (HasQual)                 \
            OS << ", ";                \
          HasQual = true;              \
          OS << Text;                  \
        }                              \
      } while(false)

      OS << '(';
      APPEND_QUAL(QS & LValueType::Qual::Implicit, "implicit");
      APPEND_QUAL(QS & LValueType::Qual::NonSettable, "nonsettable");
      OS << ')';

#undef APPEND_QUAL
    }
    visit(T->getObjectType());
  }

  void visitArchetypeType(ArchetypeType *T) {
    OS << T->getFullName();
  }

  void visitGenericTypeParamType(GenericTypeParamType *T) {
    if (auto decl = T->getDecl()) {
      auto Name = decl->getName();
      if (Name.empty())
        OS << "<anonymous>";
      else
        OS << Name.str();
      return;
    }

    OS << "$T_" << T->getDepth() << "_" << T->getIndex();
  }

  void visitAssociatedTypeType(AssociatedTypeType *T) {
    auto Name = T->getDecl()->getName();
    if (Name.empty())
      OS << "<anonymous>";
    else
      OS << Name.str();
  }

  void visitSubstitutedType(SubstitutedType *T) {
    visit(T->getReplacementType());
  }

  void visitDependentMemberType(DependentMemberType *T) {
    visit(T->getBase());
    OS << "." << T->getName().str();
  }

  void visitUnownedStorageType(UnownedStorageType *T) {
    OS << "@sil_unowned ";
    visit(T->getReferentType());
  }

  void visitWeakStorageType(WeakStorageType *T) {
    OS << "@sil_weak ";
    visit(T->getReferentType());
  }

  void visitTypeVariableType(TypeVariableType *T) {
    OS << "$T" << T->getID();
  }
};
} // unnamed namespace

void Type::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}
void Type::print(raw_ostream &OS, const PrintOptions &PO) const {
  if (isNull())
    OS << "<null>";
  else
    TypePrinter(OS, PO).visit(*this);
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

void SILParameterInfo::dump() const {
  print(llvm::errs());
}
void SILParameterInfo::print(raw_ostream &out, const PrintOptions &opts) const {
  out << getStringForParameterConvention(getConvention());
  getType().print(out, opts);
}

static StringRef getStringForResultConvention(ResultConvention conv) {
  switch (conv) {
  case ResultConvention::Owned: return "@owned ";
  case ResultConvention::Unowned: return "";
  case ResultConvention::Autoreleased: return "@autoreleased ";
  }
  llvm_unreachable("bad result convention");
}

void SILResultInfo::dump() const {
  print(llvm::errs());
}
void SILResultInfo::print(raw_ostream &out, const PrintOptions &opts) const {
  out << getStringForResultConvention(getConvention());
  getType().print(out, opts);
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

