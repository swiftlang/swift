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
#include "PrintClangValueType.h"
#include "SwiftToClangInteropContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeVisitor.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/IRGen/IRABIDetailsProvider.h"
#include "llvm/ADT/STLExtras.h"

using namespace swift;

namespace {

enum class FunctionSignatureTypeUse { ParamType, ReturnType };

Optional<PrimitiveTypeMapping::ClangTypeInfo>
getKnownTypeInfo(const TypeDecl *typeDecl, PrimitiveTypeMapping &typeMapping,
                 OutputLanguageMode languageMode) {
  return languageMode == OutputLanguageMode::Cxx
             ? typeMapping.getKnownCxxTypeInfo(typeDecl)
             : typeMapping.getKnownCTypeInfo(typeDecl);
}

bool isKnownType(Type t, PrimitiveTypeMapping &typeMapping,
                 OutputLanguageMode languageMode) {
  const TypeDecl *typeDecl;
  if (auto *typeAliasType = dyn_cast<TypeAliasType>(t.getPointer()))
    typeDecl = typeAliasType->getDecl();
  else if (auto *structDecl = t->getStructOrBoundGenericStruct())
    typeDecl = structDecl;
  else
    return false;
  return getKnownTypeInfo(typeDecl, typeMapping, languageMode) != None;
}

bool isKnownCxxType(Type t, PrimitiveTypeMapping &typeMapping) {
  return isKnownType(t, typeMapping, OutputLanguageMode::Cxx);
}

bool isKnownCType(Type t, PrimitiveTypeMapping &typeMapping) {
  return isKnownType(t, typeMapping, OutputLanguageMode::ObjC);
}

// Prints types in the C function signature that corresponds to the
// native Swift function/method.
class CFunctionSignatureTypePrinter
    : public TypeVisitor<CFunctionSignatureTypePrinter, void,
                         Optional<OptionalTypeKind>>,
      private ClangSyntaxPrinter {
public:
  CFunctionSignatureTypePrinter(raw_ostream &os, raw_ostream &cPrologueOS,
                                PrimitiveTypeMapping &typeMapping,
                                OutputLanguageMode languageMode,
                                SwiftToClangInteropContext &interopContext,
                                FunctionSignatureTypeUse typeUseKind =
                                    FunctionSignatureTypeUse::ParamType)
      : ClangSyntaxPrinter(os), cPrologueOS(cPrologueOS),
        typeMapping(typeMapping), interopContext(interopContext),
        languageMode(languageMode), typeUseKind(typeUseKind) {}

  bool printIfKnownSimpleType(const TypeDecl *typeDecl,
                              Optional<OptionalTypeKind> optionalKind) {
    auto knownTypeInfo = getKnownTypeInfo(typeDecl, typeMapping, languageMode);
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
    // FIXME: Handle optional structures.
    if (typeUseKind == FunctionSignatureTypeUse::ParamType) {
      if (languageMode != OutputLanguageMode::Cxx &&
          interopContext.getIrABIDetails().shouldPassIndirectly(ST)) {
        // FIXME: it would be nice to print out the C struct type here.
        os << "const void * _Nonnull";
      } else {
        ClangValueTypePrinter(os, cPrologueOS, typeMapping, interopContext)
            .printValueTypeParameterType(SD, languageMode);
      }
    } else
      ClangValueTypePrinter(os, cPrologueOS, typeMapping, interopContext)
          .printValueTypeReturnType(SD, languageMode);
  }

  void visitPart(Type Ty, Optional<OptionalTypeKind> optionalKind) {
    TypeVisitor::visit(Ty, optionalKind);
  }

private:
  raw_ostream &cPrologueOS;
  PrimitiveTypeMapping &typeMapping;
  SwiftToClangInteropContext &interopContext;
  OutputLanguageMode languageMode;
  FunctionSignatureTypeUse typeUseKind;
};

} // end namespace

void DeclAndTypeClangFunctionPrinter::printFunctionSignature(
    FuncDecl *FD, StringRef name, Type resultTy, FunctionSignatureKind kind) {
  OutputLanguageMode outputLang = kind == FunctionSignatureKind::CFunctionProto
                                      ? OutputLanguageMode::ObjC
                                      : OutputLanguageMode::Cxx;
  // FIXME: Might need a PrintMultiPartType here.
  auto print = [&, this](Type ty, Optional<OptionalTypeKind> optionalKind,
                         StringRef name, bool isInOutParam) {
    // FIXME: add support for noescape and PrintMultiPartType,
    // see DeclAndTypePrinter::print.
    CFunctionSignatureTypePrinter typePrinter(os, cPrologueOS, typeMapping,
                                              outputLang, interopContext);
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
  bool isIndirectReturnType =
      kind == FunctionSignatureKind::CFunctionProto &&
      !isKnownCType(resultTy, typeMapping) &&
      interopContext.getIrABIDetails().shouldReturnIndirectly(resultTy);
  if (!isIndirectReturnType) {
    OptionalTypeKind retKind;
    Type objTy;
    std::tie(objTy, retKind) =
        DeclAndTypePrinter::getObjectTypeAndOptionality(FD, resultTy);
    CFunctionSignatureTypePrinter typePrinter(
        os, cPrologueOS, typeMapping, outputLang, interopContext,
        FunctionSignatureTypeUse::ReturnType);
    typePrinter.visit(objTy, retKind);
  } else {
    os << "void";
  }

  os << ' ' << name << '(';

  bool HasParams = false;
  // Indirect result is passed in as the first parameter.
  if (isIndirectReturnType) {
    assert(kind == FunctionSignatureKind::CFunctionProto);
    HasParams = true;
    // FIXME: it would be nice to print out the C struct type here.
    os << "SWIFT_INDIRECT_RESULT void * _Nonnull";
  }
  // Print out the parameter types.
  auto params = FD->getParameters();
  if (params->size()) {
    if (HasParams)
      os << ", ";
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
  } else if (kind == FunctionSignatureKind::CFunctionProto && !HasParams) {
    // Emit 'void' in an empty parameter list for C function declarations.
    os << "void";
  }
  os << ')';
}

void DeclAndTypeClangFunctionPrinter::printCxxToCFunctionParameterUse(
    const ParamDecl *param, StringRef name) {
  auto namePrinter = [&]() { ClangSyntaxPrinter(os).printIdentifier(name); };
  auto type = param->getType();
  if (!isKnownCxxType(type, typeMapping)) {
    if (auto *structDecl = type->getStructOrBoundGenericStruct()) {
      ClangValueTypePrinter(os, cPrologueOS, typeMapping, interopContext)
          .printParameterCxxToCUseScaffold(
              interopContext.getIrABIDetails().shouldPassIndirectly(type),
              structDecl, namePrinter);
      return;
    }
  }
  // Primitive types are passed directly without any conversions.
  namePrinter();
}

void DeclAndTypeClangFunctionPrinter::printCxxThunkBody(
    StringRef swiftSymbolName, Type resultTy, ParameterList *params) {
  auto printCallToCFunc = [&](Optional<StringRef> additionalParam) {
    os << cxx_synthesis::getCxxImplNamespaceName() << "::" << swiftSymbolName
       << '(';

    bool hasParams = false;
    if (additionalParam) {
      hasParams = true;
      os << *additionalParam;
    }

    if (params->size()) {
      if (hasParams)
        os << ", ";
      size_t index = 1;
      interleaveComma(*params, os, [&](const ParamDecl *param) {
        if (param->isInOut()) {
          os << "&";
        }

        if (param->hasName()) {
          printCxxToCFunctionParameterUse(param, param->getName().str());
        } else {
          std::string paramName;
          llvm::raw_string_ostream paramOS(paramName);
          paramOS << "_" << index;
          printCxxToCFunctionParameterUse(param, paramOS.str());
        }
        ++index;
      });
    }

    os << ')';
  };

  // Values types are returned either direcly in their C representation, or
  // indirectly by a pointer.
  if (!isKnownCxxType(resultTy, typeMapping)) {
    if (auto *structDecl = resultTy->getStructOrBoundGenericStruct()) {
      bool isIndirect =
          interopContext.getIrABIDetails().shouldReturnIndirectly(resultTy);
      ClangValueTypePrinter valueTypePrinter(os, cPrologueOS, typeMapping,
                                             interopContext);
      if (isIndirect) {
        valueTypePrinter.printValueTypeIndirectReturnScaffold(
            structDecl, [&](StringRef returnParam) {
              printCallToCFunc(/*additionalParam=*/returnParam);
            });
      } else {
        valueTypePrinter.printValueTypeDirectReturnScaffold(
            structDecl, [&]() { printCallToCFunc(/*additionalParam=*/None); });
      }
      return;
    }
  }

  // Primitive values are returned directly without any conversions.
  os << "  return ";
  printCallToCFunc(/*additionalParam=*/None);
  os << ";\n";
}
