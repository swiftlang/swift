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
#include "OutputLanguageMode.h"
#include "PrimitiveTypeMapping.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeVisitor.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "llvm/ADT/STLExtras.h"

using namespace swift;

namespace {

// Prints types in the C function signature that corresponds to the
// native Swift function/method.
class CFunctionSignatureTypePrinter
    : public TypeVisitor<CFunctionSignatureTypePrinter, void,
                         Optional<OptionalTypeKind>>,
      private ClangSyntaxPrinter {
public:
  CFunctionSignatureTypePrinter(raw_ostream &os,
                                PrimitiveTypeMapping &typeMapping,
                                OutputLanguageMode languageMode)
      : ClangSyntaxPrinter(os), typeMapping(typeMapping),
        languageMode(languageMode) {}

  bool printIfKnownSimpleType(const TypeDecl *typeDecl,
                              Optional<OptionalTypeKind> optionalKind) {
    auto knownTypeInfo = languageMode == OutputLanguageMode::Cxx
                             ? typeMapping.getKnownCxxTypeInfo(typeDecl)
                             : typeMapping.getKnownCTypeInfo(typeDecl);
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
  OutputLanguageMode languageMode;
};

} // end namespace

void DeclAndTypeClangFunctionPrinter::printFunctionSignature(
    FuncDecl *FD, StringRef name, Type resultTy,
    ABIExpansionParams &additionalParams,
    FunctionSignatureKind kind) {
  OutputLanguageMode outputLang = kind == FunctionSignatureKind::CFunctionProto
                                      ? OutputLanguageMode::ObjC
                                      : OutputLanguageMode::Cxx;
  // FIXME: Might need a PrintMultiPartType here.
  auto print = [&, this](Type ty, Optional<OptionalTypeKind> optionalKind,
                         StringRef name, bool isInOutParam) {
    // FIXME: add support for noescape and PrintMultiPartType,
    // see DeclAndTypePrinter::print.
    CFunctionSignatureTypePrinter typePrinter(os, typeMapping, outputLang);
    typePrinter.visit(ty, optionalKind);

    if (isInOutParam) {
      os << (outputLang == OutputLanguageMode::Cxx ? " &" : " *");
    }

    if (!name.empty()) {
      os << ' ';
      ClangSyntaxPrinter(os).printIdentifier(name);
    }
  };

  // Print out the return type.
  OptionalTypeKind retKind;
  Type objTy;
  std::tie(objTy, retKind) =
      DeclAndTypePrinter::getObjectTypeAndOptionality(FD, resultTy);
  CFunctionSignatureTypePrinter typePrinter(os, typeMapping, outputLang);
  typePrinter.visit(objTy, retKind);

  os << ' ' << name << '(';

  // Print out the parameter types.
  auto params = FD->getParameters();
  if (kind == FunctionSignatureKind::CFunctionProto && FD->hasThrows())
    additionalParams.push_back(
        IRABIDetailsProvider::ABIParameter {
            IRABIDetailsProvider::ABIParameterRole::Error,
            resultTy->getASTContext().getOpaquePointerDecl()
        });

  if (params->size()) {
    size_t paramIndex = 1;
    llvm::interleaveComma(*params, os, [&](const ParamDecl *param) {
      OptionalTypeKind argKind;
      Type objTy;
      std::tie(objTy, argKind) =
          DeclAndTypePrinter::getObjectTypeAndOptionality(
              param, param->getInterfaceType());
      std::string paramName =
          param->getName().empty() ? "" : param->getName().str().str();
      // Always emit a named parameter for the C++ inline thunk to ensure it can
      // be referenced in the body.
      if (kind == FunctionSignatureKind::CxxInlineThunk && paramName.empty()) {
        llvm::raw_string_ostream os(paramName);
        os << "_" << paramIndex;
      }
      print(objTy, argKind, paramName, param->isInOut());
      ++paramIndex;
    });
  } else if (!additionalParams.empty()) {
    llvm::interleaveComma(additionalParams, os,
                          [&](const IRABIDetailsProvider::ABIParameter param) {
      auto newType = typeMapping.getKnownCxxTypeInfo(param.typeDecl);
      os << newType->name;
    });
  } else if (kind == FunctionSignatureKind::CFunctionProto) {
    // Emit 'void' in an empty parameter list for C function declarations.
    os << "void";
  }
  os << ')';
}
