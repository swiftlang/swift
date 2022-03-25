//===--- PrintClangFunction.cpp - Printer for C/C++ functions ---*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "PrintClangFunction.h"
#include "ClangSyntaxPrinter.h"
#include "DeclAndTypePrinter.h"
#include "PrimitiveTypeMapping.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeVisitor.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "llvm/ADT/STLExtras.h"

using namespace swift;

// Prints types in the C function signature that corresponds to the
// native Swift function/method.
class CFunctionSignatureTypePrinter
    : public TypeVisitor<CFunctionSignatureTypePrinter, void,
                         Optional<OptionalTypeKind>>,
      private ClangSyntaxPrinter {
public:
  CFunctionSignatureTypePrinter(raw_ostream &os,
                                PrimitiveTypeMapping &typeMapping)
      : ClangSyntaxPrinter(os), typeMapping(typeMapping) {}

  bool printIfKnownSimpleType(const TypeDecl *typeDecl,
                              Optional<OptionalTypeKind> optionalKind) {
    auto knownTypeInfo = typeMapping.getKnownCTypeInfo(typeDecl);
    if (!knownTypeInfo)
      return false;
    os << knownTypeInfo->name;
    if (knownTypeInfo->canBeNullable)
      printNullability(optionalKind);
    return true;
  }

  void visitType(TypeBase *Ty, Optional<OptionalTypeKind> optionalKind) {
    assert(Ty->getDesugaredType() == Ty && "unhandled sugared type");
    os << "/* ";
    Ty->print(os);
    os << " */";
  }

  void visitTupleType(TupleType *TT, Optional<OptionalTypeKind> optionalKind) {
    assert(TT->getNumElements() == 0);
    // FIXME: Handle non-void type.
    os << "void";
  }

  void visitTypeAliasType(TypeAliasType *aliasTy,
                          Optional<OptionalTypeKind> optionalKind) {
    const TypeAliasDecl *alias = aliasTy->getDecl();
    if (printIfKnownSimpleType(alias, optionalKind))
      return;

    visitPart(aliasTy->getSinglyDesugaredType(), optionalKind);
  }

  void visitStructType(StructType *ST,
                       Optional<OptionalTypeKind> optionalKind) {
    const StructDecl *SD = ST->getStructOrBoundGenericStruct();

    // Handle known type names.
    if (printIfKnownSimpleType(SD, optionalKind))
      return;
    // FIXME: Handle struct types.
  }

  void visitPart(Type Ty, Optional<OptionalTypeKind> optionalKind) {
    TypeVisitor::visit(Ty, optionalKind);
  }

private:
  PrimitiveTypeMapping &typeMapping;
};

void DeclAndTypeClangFunctionPrinter::printFunctionDeclAsCFunctionDecl(
    FuncDecl *FD, StringRef name, Type resultTy) {
  // FIXME: Might need a PrintMultiPartType here.
  auto print = [this](Type ty, Optional<OptionalTypeKind> optionalKind,
                      StringRef name) {
    // FIXME: add support for noescape and PrintMultiPartType,
    // see DeclAndTypePrinter::print.
    CFunctionSignatureTypePrinter typePrinter(os, typeMapping);
    typePrinter.visit(ty, optionalKind);

    if (!name.empty()) {
      os << ' ' << name;
      if (DeclAndTypePrinter::isStrClangKeyword(name))
        os << '_';
    }
  };

  // Print out the return type.
  OptionalTypeKind kind;
  Type objTy;
  std::tie(objTy, kind) =
      DeclAndTypePrinter::getObjectTypeAndOptionality(FD, resultTy);
  CFunctionSignatureTypePrinter typePrinter(os, typeMapping);
  typePrinter.visit(objTy, kind);

  os << ' ' << name << '(';

  // Print out the parameter types.
  auto params = FD->getParameters();
  if (params->size()) {
    llvm::interleaveComma(*params, os, [&](const ParamDecl *param) {
      OptionalTypeKind kind;
      Type objTy;
      std::tie(objTy, kind) = DeclAndTypePrinter::getObjectTypeAndOptionality(
          param, param->getInterfaceType());
      StringRef paramName =
          param->getName().empty() ? "" : param->getName().str();
      print(objTy, kind, paramName);
    });
  } else {
    os << "void";
  }
  os << ')';
}
