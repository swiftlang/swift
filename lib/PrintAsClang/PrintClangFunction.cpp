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

bool isResilientType(Type t) {
  if (auto *typeAliasType = dyn_cast<TypeAliasType>(t.getPointer()))
    return isResilientType(typeAliasType->getSinglyDesugaredType());
  else if (auto *nominalType = t->getNominalOrBoundGenericNominal())
    return nominalType->isResilient();
  return false;
}

bool isKnownCxxType(Type t, PrimitiveTypeMapping &typeMapping) {
  return isKnownType(t, typeMapping, OutputLanguageMode::Cxx);
}

bool isKnownCType(Type t, PrimitiveTypeMapping &typeMapping) {
  return isKnownType(t, typeMapping, OutputLanguageMode::ObjC);
}

struct CFunctionSignatureTypePrinterModifierDelegate {
  /// Prefix the indirect value type param being printed in C mode.
  Optional<llvm::function_ref<void(raw_ostream &)>>
      prefixIndirectParamValueTypeInC = None;
};

// Prints types in the C function signature that corresponds to the
// native Swift function/method.
class CFunctionSignatureTypePrinter
    : public TypeVisitor<CFunctionSignatureTypePrinter, void,
                         Optional<OptionalTypeKind>, bool>,
      private ClangSyntaxPrinter {
public:
  CFunctionSignatureTypePrinter(
      raw_ostream &os, raw_ostream &cPrologueOS,
      PrimitiveTypeMapping &typeMapping, OutputLanguageMode languageMode,
      SwiftToClangInteropContext &interopContext,
      CFunctionSignatureTypePrinterModifierDelegate modifiersDelegate,
      FunctionSignatureTypeUse typeUseKind =
          FunctionSignatureTypeUse::ParamType)
      : ClangSyntaxPrinter(os), cPrologueOS(cPrologueOS),
        typeMapping(typeMapping), interopContext(interopContext),
        languageMode(languageMode), modifiersDelegate(modifiersDelegate),
        typeUseKind(typeUseKind) {}

  bool printIfKnownSimpleType(const TypeDecl *typeDecl,
                              Optional<OptionalTypeKind> optionalKind,
                              bool isInOutParam) {
    auto knownTypeInfo = getKnownTypeInfo(typeDecl, typeMapping, languageMode);
    if (!knownTypeInfo)
      return false;
    os << knownTypeInfo->name;
    if (knownTypeInfo->canBeNullable) {
      printNullability(optionalKind);
    }
    if (isInOutParam) {
      os << (languageMode == swift::OutputLanguageMode::Cxx ? " &" : " *");
    }
    return true;
  }

  void visitType(TypeBase *Ty, Optional<OptionalTypeKind> optionalKind,
                 bool isInOutParam) {
    assert(Ty->getDesugaredType() == Ty && "unhandled sugared type");
    os << "/* ";
    Ty->print(os);
    os << " */";
  }

  void visitTupleType(TupleType *TT, Optional<OptionalTypeKind> optionalKind,
                      bool isInOutParam) {
    assert(TT->getNumElements() == 0);
    // FIXME: Handle non-void type.
    os << "void";
  }

  void visitTypeAliasType(TypeAliasType *aliasTy,
                          Optional<OptionalTypeKind> optionalKind,
                          bool isInOutParam) {
    const TypeAliasDecl *alias = aliasTy->getDecl();
    if (printIfKnownSimpleType(alias, optionalKind, isInOutParam))
      return;

    visitPart(aliasTy->getSinglyDesugaredType(), optionalKind, isInOutParam);
  }

  void visitEnumType(EnumType *ET, Optional<OptionalTypeKind> optionalKind,
                     bool isInOutParam) {
    visitValueType(ET, optionalKind, isInOutParam);
  }

  void visitStructType(StructType *ST, Optional<OptionalTypeKind> optionalKind,
                       bool isInOutParam) {
    visitValueType(ST, optionalKind, isInOutParam);
  }

  void visitValueType(NominalType *NT, Optional<OptionalTypeKind> optionalKind,
                      bool isInOutParam) {
    assert(isa<StructType>(NT) || isa<EnumType>(NT));
    const auto *decl = NT->getNominalOrBoundGenericNominal();
    assert(isa<StructDecl>(decl) || isa<EnumDecl>(decl));

    // Handle known type names.
    if (printIfKnownSimpleType(decl, optionalKind, isInOutParam))
      return;
    // FIXME: Handle optional structures.
    if (typeUseKind == FunctionSignatureTypeUse::ParamType) {
      if (languageMode != OutputLanguageMode::Cxx &&
          (decl->isResilient() ||
           interopContext.getIrABIDetails().shouldPassIndirectly(NT))) {
        if (modifiersDelegate.prefixIndirectParamValueTypeInC)
          (*modifiersDelegate.prefixIndirectParamValueTypeInC)(os);
        // FIXME: it would be nice to print out the C struct type here.
        if (isInOutParam) {
          os << "void * _Nonnull";
        } else {
          os << "const void * _Nonnull";
        }

      } else {
        ClangValueTypePrinter(os, cPrologueOS, typeMapping, interopContext)
            .printValueTypeParameterType(decl, languageMode, isInOutParam);
      }
    } else
      ClangValueTypePrinter(os, cPrologueOS, typeMapping, interopContext)
          .printValueTypeReturnType(decl, languageMode);
  }

  void visitPart(Type Ty, Optional<OptionalTypeKind> optionalKind,
                 bool isInOutParam) {
    TypeVisitor::visit(Ty, optionalKind, isInOutParam);
  }

private:
  raw_ostream &cPrologueOS;
  PrimitiveTypeMapping &typeMapping;
  SwiftToClangInteropContext &interopContext;
  OutputLanguageMode languageMode;
  CFunctionSignatureTypePrinterModifierDelegate modifiersDelegate;
  FunctionSignatureTypeUse typeUseKind;
};

} // end namespace

void DeclAndTypeClangFunctionPrinter::printFunctionSignature(
    const AbstractFunctionDecl *FD, StringRef name, Type resultTy,
    FunctionSignatureKind kind, ArrayRef<AdditionalParam> additionalParams,
    FunctionSignatureModifiers modifiers) {
  OutputLanguageMode outputLang = kind == FunctionSignatureKind::CFunctionProto
                                      ? OutputLanguageMode::ObjC
                                      : OutputLanguageMode::Cxx;
  // FIXME: Might need a PrintMultiPartType here.
  auto print =
      [&, this](Type ty, Optional<OptionalTypeKind> optionalKind,
                StringRef name, bool isInOutParam,
                CFunctionSignatureTypePrinterModifierDelegate delegate = {}) {
        // FIXME: add support for noescape and PrintMultiPartType,
        // see DeclAndTypePrinter::print.
        CFunctionSignatureTypePrinter typePrinter(
            os, cPrologueOS, typeMapping, outputLang, interopContext, delegate);
        typePrinter.visit(ty, optionalKind, isInOutParam);

        if (!name.empty()) {
          os << ' ';
          ClangSyntaxPrinter(os).printIdentifier(name);
        }
      };

  // Print out the return type.
  bool isIndirectReturnType =
      kind == FunctionSignatureKind::CFunctionProto &&
      !isKnownCType(resultTy, typeMapping) &&
      (isResilientType(resultTy) ||
       interopContext.getIrABIDetails().shouldReturnIndirectly(resultTy));
  if (!isIndirectReturnType) {
    OptionalTypeKind retKind;
    Type objTy;
    std::tie(objTy, retKind) =
        DeclAndTypePrinter::getObjectTypeAndOptionality(FD, resultTy);
    CFunctionSignatureTypePrinter typePrinter(
        os, cPrologueOS, typeMapping, outputLang, interopContext,
        CFunctionSignatureTypePrinterModifierDelegate(),
        FunctionSignatureTypeUse::ReturnType);
    // Param for indirect return cannot be marked as inout
    typePrinter.visit(objTy, retKind, /*isInOutParam=*/false);
  } else {
    os << "void";
  }

  os << ' ';
  if (modifiers.qualifierContext) {
    // FIXME: Full qualifiers for nested types?
    ClangSyntaxPrinter(os).printBaseName(modifiers.qualifierContext);
    os << "::";
  }
  ClangSyntaxPrinter(os).printIdentifier(name);
  os << '(';

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
    HasParams = true;
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
  }
  if (additionalParams.size()) {
    assert(kind == FunctionSignatureKind::CFunctionProto);
    if (HasParams)
      os << ", ";
    HasParams = true;
    interleaveComma(additionalParams, os, [&](const AdditionalParam &param) {
      if (param.role == AdditionalParam::Role::Self) {
        CFunctionSignatureTypePrinterModifierDelegate delegate;
        delegate.prefixIndirectParamValueTypeInC = [](raw_ostream &os) {
          os << "SWIFT_CONTEXT ";
        };
        if (FD->hasThrows())
          os << "SWIFT_CONTEXT ";
        if (param.isIndirect) {
          (*delegate.prefixIndirectParamValueTypeInC)(os);
          os << "void * _Nonnull _self";
        } else {
          print(param.type, OptionalTypeKind::OTK_None, "_self",
                /*isInOut*/ false, delegate);
        }
      } else if (param.role ==  AdditionalParam::Role::Error) {
        os << "SWIFT_ERROR_RESULT ";
        os << "void ** _error";
      }
    });
  }
  if (kind == FunctionSignatureKind::CFunctionProto && !HasParams) {
    // Emit 'void' in an empty parameter list for C function declarations.
    os << "void";
  }
  os << ')';
}

void DeclAndTypeClangFunctionPrinter::printCxxToCFunctionParameterUse(
    Type type, StringRef name, bool isInOut, bool isIndirect,
    llvm::Optional<AdditionalParam::Role> paramRole) {
  auto namePrinter = [&]() { ClangSyntaxPrinter(os).printIdentifier(name); };
  if (!isKnownCxxType(type, typeMapping) &&
      !hasKnownOptionalNullableCxxMapping(type)) {
    if (auto *decl = type->getNominalOrBoundGenericNominal()) {
      if ((isa<StructDecl>(decl) || isa<EnumDecl>(decl))) {
        ClangValueTypePrinter(os, cPrologueOS, typeMapping, interopContext)
            .printParameterCxxToCUseScaffold(
                isIndirect || decl->isResilient() ||
                    interopContext.getIrABIDetails().shouldPassIndirectly(type),
                decl, namePrinter, isInOut,
                /*isSelf=*/paramRole &&
                    *paramRole == AdditionalParam::Role::Self);
        return;
      }
    }
  }
  // Primitive types are passed directly without any conversions.
  if (isInOut) {
    os << "&";
  }
  namePrinter();
}

void DeclAndTypeClangFunctionPrinter::printCxxToCFunctionParameterUse(
    const ParamDecl *param, StringRef name) {
  printCxxToCFunctionParameterUse(param->getType(), name, param->isInOut());
}

void DeclAndTypeClangFunctionPrinter::printCxxThunkBody(
    StringRef swiftSymbolName, Type resultTy, const ParameterList *params,
    ArrayRef<AdditionalParam> additionalParams, bool hasThrows) {
  if (hasThrows) {
    os << "  void* opaqueError = nullptr;\n";
    os << "  void* self = nullptr;\n";
  }
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
      hasParams = true;
      size_t index = 1;
      interleaveComma(*params, os, [&](const ParamDecl *param) {
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

    if (additionalParams.size()) {
      if (hasParams)
        os << ", ";
      interleaveComma(additionalParams, os, [&](const AdditionalParam &param) {
         if (param.role == AdditionalParam::Role::Self && !hasThrows)
          printCxxToCFunctionParameterUse(param.type, "*this", /*isInOut=*/false,
                                          /*isIndirect=*/param.isIndirect,
                                          param.role);
         else if(param.role == AdditionalParam::Role::Self && hasThrows)
            printCxxToCFunctionParameterUse(param.type, "self", /*isInOut=*/false,
                                            /*isIndirect=*/param.isIndirect,
                                            param.role);
         else if(param.role == AdditionalParam::Role::Error && hasThrows)
           printCxxToCFunctionParameterUse(param.type, "&opaqueError", /*isInOut=*/false,
                                           /*isIndirect=*/param.isIndirect,
                                           param.role);
      });
    }

    os << ')';
  };

  // Values types are returned either direcly in their C representation, or
  // indirectly by a pointer.
  if (!isKnownCxxType(resultTy, typeMapping) &&
      !hasKnownOptionalNullableCxxMapping(resultTy)) {
    if (auto *decl = resultTy->getNominalOrBoundGenericNominal()) {
      if ((isa<StructDecl>(decl) || isa<EnumDecl>(decl))) {
        bool isIndirect =
            decl->isResilient() ||
            interopContext.getIrABIDetails().shouldReturnIndirectly(resultTy);
        ClangValueTypePrinter valueTypePrinter(os, cPrologueOS, typeMapping,
                                               interopContext);
        if (isIndirect) {
          valueTypePrinter.printValueTypeIndirectReturnScaffold(
              decl, [&](StringRef returnParam) {
                printCallToCFunc(/*additionalParam=*/returnParam);
              });
        } else {
          valueTypePrinter.printValueTypeDirectReturnScaffold(
              decl, [&]() { printCallToCFunc(/*additionalParam=*/None); });
        }
        return;
      }
    }
  }

  // Primitive values are returned directly without any conversions.
  os << "  return ";
  printCallToCFunc(/*additionalParam=*/None);
  os << ";\n";
}

void DeclAndTypeClangFunctionPrinter::printCxxMethod(
    const NominalTypeDecl *typeDeclContext, const AbstractFunctionDecl *FD,
    StringRef swiftSymbolName, Type resultTy, bool isDefinition) {
  bool isConstructor = isa<ConstructorDecl>(FD);
  os << "  ";
  if (isConstructor && !isDefinition)
    os << "static ";
  os << "inline ";
  // FIXME: Full qualifier.
  FunctionSignatureModifiers modifiers;
  if (isDefinition)
    modifiers.qualifierContext = typeDeclContext;
  printFunctionSignature(
      FD, isConstructor ? "init" : FD->getName().getBaseIdentifier().get(),
      resultTy, FunctionSignatureKind::CxxInlineThunk, {}, modifiers);
  bool isMutating = false;
  if (auto *funcDecl = dyn_cast<FuncDecl>(FD))
    isMutating = funcDecl->isMutating();
  if (!isMutating && !isConstructor)
    os << " const";
  if (!isDefinition) {
    os << ";\n";
    return;
  }

  os << " {\n";
  // FIXME: should it be objTy for resultTy?
  SmallVector<AdditionalParam, 2> additionalParams;
  if (!isConstructor)
    additionalParams.push_back(AdditionalParam{
        AdditionalParam::Role::Self,
        typeDeclContext->getDeclaredType(),
        /*isIndirect=*/isMutating,
    });
  printCxxThunkBody(swiftSymbolName, resultTy, FD->getParameters(),
                    additionalParams, FD->hasThrows());
  os << "  }\n";
}

void DeclAndTypeClangFunctionPrinter::printCxxPropertyAccessorMethod(
    const NominalTypeDecl *typeDeclContext, const AccessorDecl *accessor,
    StringRef swiftSymbolName, Type resultTy, bool isDefinition) {
  assert(accessor->getParameters()->size() == 0);
  os << "  inline ";

  OptionalTypeKind retKind;
  Type objTy;
  std::tie(objTy, retKind) =
      DeclAndTypePrinter::getObjectTypeAndOptionality(accessor, resultTy);
  CFunctionSignatureTypePrinter typePrinter(
      os, cPrologueOS, typeMapping, OutputLanguageMode::Cxx, interopContext,
      CFunctionSignatureTypePrinterModifierDelegate(),
      FunctionSignatureTypeUse::ReturnType);
  typePrinter.visit(objTy, retKind, /*isInOut=*/false);

  ClangSyntaxPrinter printer(os);
  os << ' ';
  if (isDefinition) {
    // FIXME: Full qualifiers for nested types?
    printer.printBaseName(typeDeclContext);
    os << "::";
  }

  StringRef name;
  // For a getter or setter, go through the variable or subscript decl.
  name = accessor->getStorage()->getBaseIdentifier().str();

  // FIXME: some names are remapped differently. (e.g. isX).
  os << "get" << char(std::toupper(name[0])) << name.drop_front();
  os << "() const";
  if (!isDefinition) {
    os << ";\n";
    return;
  }
  os << " {\n";
  // FIXME: should it be objTy for resultTy?
  printCxxThunkBody(swiftSymbolName, resultTy, accessor->getParameters(),
                    {AdditionalParam{AdditionalParam::Role::Self,
                                     typeDeclContext->getDeclaredType(),
                                     /*isIndirect=*/false}});
  os << "  }\n";
}

bool DeclAndTypeClangFunctionPrinter::hasKnownOptionalNullableCxxMapping(
    Type type) {
  if (auto optionalObjectType = type->getOptionalObjectType()) {
    if (auto typeInfo = typeMapping.getKnownCxxTypeInfo(
            optionalObjectType->getNominalOrBoundGenericNominal())) {
      return typeInfo->canBeNullable;
    }
  }
  return false;
}
