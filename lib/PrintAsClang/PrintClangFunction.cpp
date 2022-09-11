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
#include "PrintClangClassType.h"
#include "PrintClangValueType.h"
#include "SwiftToClangInteropContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericParamList.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/SwiftNameTranslation.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeVisitor.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/IRGen/IRABIDetailsProvider.h"
#include "llvm/ADT/STLExtras.h"

using namespace swift;

namespace {

// FIXME: RENAME.
enum class FunctionSignatureTypeUse { TypeReference, ParamType, ReturnType };

Optional<PrimitiveTypeMapping::ClangTypeInfo>
getKnownTypeInfo(const TypeDecl *typeDecl, PrimitiveTypeMapping &typeMapping,
                 OutputLanguageMode languageMode) {
  return languageMode == OutputLanguageMode::Cxx
             ? typeMapping.getKnownCxxTypeInfo(typeDecl)
             : typeMapping.getKnownCTypeInfo(typeDecl);
}

bool isKnownType(Type t, PrimitiveTypeMapping &typeMapping,
                 OutputLanguageMode languageMode) {
  if (auto *typeAliasType = dyn_cast<TypeAliasType>(t.getPointer())) {
    auto aliasInfo =
        getKnownTypeInfo(typeAliasType->getDecl(), typeMapping, languageMode);
    if (aliasInfo != None)
      return true;
    return isKnownType(typeAliasType->getSinglyDesugaredType(), typeMapping,
                       languageMode);
  }

  const TypeDecl *typeDecl;
  auto *tPtr = t->isOptional() ? t->getOptionalObjectType()->getDesugaredType()
                               : t->getDesugaredType();
  if (auto *bgt = dyn_cast<BoundGenericStructType>(tPtr)) {
    return bgt->isUnsafePointer() || bgt->isUnsafeMutablePointer();
  }
  if (auto *structType = dyn_cast<StructType>(tPtr)) {
    auto nullableInfo =
        getKnownTypeInfo(structType->getDecl(), typeMapping, languageMode);
    if (nullableInfo && nullableInfo->canBeNullable)
      return true;
  }

  if (auto *structDecl = t->getStructOrBoundGenericStruct())
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

struct CFunctionSignatureTypePrinterModifierDelegate {
  /// Prefix the initially printed value type.
  Optional<llvm::function_ref<ClangValueTypePrinter::TypeUseKind(
      ClangValueTypePrinter::TypeUseKind)>>
      mapValueTypeUseKind = None;
};

// Prints types in the C function signature that corresponds to the
// native Swift function/method.
class CFunctionSignatureTypePrinter
    : public TypeVisitor<CFunctionSignatureTypePrinter, ClangRepresentation,
                         Optional<OptionalTypeKind>, bool>,
      private ClangSyntaxPrinter {
public:
  CFunctionSignatureTypePrinter(
      raw_ostream &os, raw_ostream &cPrologueOS,
      PrimitiveTypeMapping &typeMapping, OutputLanguageMode languageMode,
      SwiftToClangInteropContext &interopContext,
      CFunctionSignatureTypePrinterModifierDelegate modifiersDelegate,
      const ModuleDecl *moduleContext, DeclAndTypePrinter &declPrinter,
      FunctionSignatureTypeUse typeUseKind =
          FunctionSignatureTypeUse::ParamType)
      : ClangSyntaxPrinter(os), cPrologueOS(cPrologueOS),
        typeMapping(typeMapping), interopContext(interopContext),
        languageMode(languageMode), modifiersDelegate(modifiersDelegate),
        moduleContext(moduleContext), declPrinter(declPrinter),
        typeUseKind(typeUseKind) {}

  void printInoutTypeModifier() {
    os << (languageMode == swift::OutputLanguageMode::Cxx ? " &"
                                                          : " * _Nonnull");
  }

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
    if (isInOutParam)
      printInoutTypeModifier();
    return true;
  }

  ClangRepresentation visitType(TypeBase *Ty,
                                Optional<OptionalTypeKind> optionalKind,
                                bool isInOutParam) {
    assert(Ty->getDesugaredType() == Ty && "unhandled sugared type");
    os << "/* ";
    Ty->print(os);
    os << " */";
    return ClangRepresentation::unsupported;
  }

  ClangRepresentation visitTupleType(TupleType *TT,
                                     Optional<OptionalTypeKind> optionalKind,
                                     bool isInOutParam) {
    assert(TT->getNumElements() == 0);
    // FIXME: Handle non-void type.
    os << "void";
    return ClangRepresentation::representable;
  }

  ClangRepresentation
  visitTypeAliasType(TypeAliasType *aliasTy,
                     Optional<OptionalTypeKind> optionalKind,
                     bool isInOutParam) {
    const TypeAliasDecl *alias = aliasTy->getDecl();
    if (printIfKnownSimpleType(alias, optionalKind, isInOutParam))
      return ClangRepresentation::representable;

    return visitSugarType(aliasTy, optionalKind, isInOutParam);
  }

  ClangRepresentation visitSugarType(SugarType *sugarTy,
                                     Optional<OptionalTypeKind> optionalKind,
                                     bool isInOutParam) {
    return visitPart(sugarTy->getSinglyDesugaredType(), optionalKind,
                     isInOutParam);
  }

  ClangRepresentation visitClassType(ClassType *CT,
                                     Optional<OptionalTypeKind> optionalKind,
                                     bool isInOutParam) {
    // FIXME: handle optionalKind.
    if (languageMode != OutputLanguageMode::Cxx) {
      os << "void * _Nonnull";
      if (isInOutParam)
        os << " * _Nonnull";
      return ClangRepresentation::representable;
    }
    if (typeUseKind == FunctionSignatureTypeUse::ParamType && !isInOutParam)
      os << "const ";
    ClangSyntaxPrinter(os).printBaseName(CT->getDecl());
    if (typeUseKind == FunctionSignatureTypeUse::ParamType)
      os << "&";
    return ClangRepresentation::representable;
  }

  ClangRepresentation visitEnumType(EnumType *ET,
                                    Optional<OptionalTypeKind> optionalKind,
                                    bool isInOutParam) {
    return visitValueType(ET, ET->getNominalOrBoundGenericNominal(),
                          optionalKind, isInOutParam);
  }

  ClangRepresentation visitStructType(StructType *ST,
                                      Optional<OptionalTypeKind> optionalKind,
                                      bool isInOutParam) {
    return visitValueType(ST, ST->getNominalOrBoundGenericNominal(),
                          optionalKind, isInOutParam);
  }

  ClangRepresentation visitGenericArgs(ArrayRef<Type> genericArgs) {
    if (genericArgs.empty())
      return ClangRepresentation::representable;
    os << '<';
    llvm::SaveAndRestore<FunctionSignatureTypeUse> typeUseNormal(
        typeUseKind, FunctionSignatureTypeUse::TypeReference);
    decltype(modifiersDelegate) emptyModifiersDelegate;
    llvm::SaveAndRestore<decltype(modifiersDelegate)> modReset(
        modifiersDelegate, emptyModifiersDelegate);
    ClangRepresentation result = ClangRepresentation::representable;
    llvm::interleaveComma(genericArgs, os, [&](Type t) {
      result.merge(visitPart(t, None, false));
    });
    os << '>';
    return result;
  }

  ClangRepresentation visitValueType(TypeBase *type,
                                     const NominalTypeDecl *decl,
                                     Optional<OptionalTypeKind> optionalKind,
                                     bool isInOutParam,
                                     ArrayRef<Type> genericArgs = {}) {
    assert(isa<StructDecl>(decl) || isa<EnumDecl>(decl));

    // Handle known type names.
    if (printIfKnownSimpleType(decl, optionalKind, isInOutParam))
      return ClangRepresentation::representable;
    if (!declPrinter.shouldInclude(decl))
      return ClangRepresentation::unsupported; // FIXME: propagate why it's not
                                               // exposed.

    // Only C++ mode supports struct types.
    if (languageMode != OutputLanguageMode::Cxx)
      return ClangRepresentation::unsupported;

    // FIXME: Handle optional structures.
    if (typeUseKind == FunctionSignatureTypeUse::ParamType) {
      if (!isInOutParam) {
        os << "const ";
      }
      ClangSyntaxPrinter(os).printPrimaryCxxTypeName(decl, moduleContext);
      auto result = visitGenericArgs(genericArgs);
      os << '&';
      return result;
    } else {
      ClangValueTypePrinter printer(os, cPrologueOS, interopContext);
      printer.printValueTypeReturnType(
          decl, languageMode,
          modifiersDelegate.mapValueTypeUseKind
              ? (*modifiersDelegate.mapValueTypeUseKind)(
                    ClangValueTypePrinter::TypeUseKind::CxxTypeName)
              : ClangValueTypePrinter::TypeUseKind::CxxTypeName,
          moduleContext);
      return visitGenericArgs(genericArgs);
    }
    return ClangRepresentation::representable;
  }

  bool printIfKnownGenericStruct(const BoundGenericStructType *BGT,
                                 Optional<OptionalTypeKind> optionalKind,
                                 bool isInOutParam) {
    auto bgsTy = Type(const_cast<BoundGenericStructType *>(BGT));
    bool isConst;
    if (bgsTy->isUnsafePointer())
      isConst = true;
    else if (bgsTy->isUnsafeMutablePointer())
      isConst = false;
    else
      return false;

    auto args = BGT->getGenericArgs();
    assert(args.size() == 1);
    visitPart(args.front(), OTK_None, /*isInOutParam=*/false);
    if (isConst)
      os << " const";
    os << " *";
    printNullability(optionalKind);
    if (isInOutParam)
      printInoutTypeModifier();
    return true;
  }

  ClangRepresentation
  visitBoundGenericStructType(BoundGenericStructType *BGT,
                              Optional<OptionalTypeKind> optionalKind,
                              bool isInOutParam) {
    if (printIfKnownGenericStruct(BGT, optionalKind, isInOutParam))
      return ClangRepresentation::representable;
    return visitValueType(BGT, BGT->getDecl(), optionalKind, isInOutParam,
                          BGT->getGenericArgs());
  }

  ClangRepresentation
  visitGenericTypeParamType(GenericTypeParamType *genericTpt,
                            Optional<OptionalTypeKind> optionalKind,
                            bool isInOutParam) {
    bool isParam = typeUseKind == FunctionSignatureTypeUse::ParamType;
    if (isParam && !isInOutParam)
      os << "const ";
    // FIXME: handle optionalKind.
    if (languageMode != OutputLanguageMode::Cxx) {
      assert(typeUseKind == FunctionSignatureTypeUse::ParamType);
      // Pass an opaque param in C mode.
      os << "void * _Nonnull";
      return ClangRepresentation::representable;
    }
    ClangSyntaxPrinter(os).printGenericTypeParamTypeName(genericTpt);
    // Pass a reference to the template type.
    if (isParam)
      os << '&';
    return ClangRepresentation::representable;
  }

  ClangRepresentation visitPart(Type Ty,
                                Optional<OptionalTypeKind> optionalKind,
                                bool isInOutParam) {
    return TypeVisitor::visit(Ty, optionalKind, isInOutParam);
  }

private:
  raw_ostream &cPrologueOS;
  PrimitiveTypeMapping &typeMapping;
  SwiftToClangInteropContext &interopContext;
  OutputLanguageMode languageMode;
  CFunctionSignatureTypePrinterModifierDelegate modifiersDelegate;
  const ModuleDecl *moduleContext;
  DeclAndTypePrinter &declPrinter;
  FunctionSignatureTypeUse typeUseKind;
};

} // end namespace

ClangRepresentation
DeclAndTypeClangFunctionPrinter::printClangFunctionReturnType(
    Type ty, OptionalTypeKind optKind, ModuleDecl *moduleContext,
    OutputLanguageMode outputLang) {
  CFunctionSignatureTypePrinter typePrinter(
      os, cPrologueOS, typeMapping, outputLang, interopContext,
      CFunctionSignatureTypePrinterModifierDelegate(), moduleContext,
      declPrinter, FunctionSignatureTypeUse::ReturnType);
  // Param for indirect return cannot be marked as inout
  return typePrinter.visit(ty, optKind, /*isInOutParam=*/false);
}

static void addABIRecordToTypeEncoding(llvm::raw_ostream &typeEncodingOS,
                                       clang::CharUnits offset,
                                       clang::CharUnits end, Type t,
                                       PrimitiveTypeMapping &typeMapping) {
  auto info =
      typeMapping.getKnownCTypeInfo(t->getNominalOrBoundGenericNominal());
  assert(info);
  typeEncodingOS << '_';
  for (char c : info->name) {
    if (c == ' ')
      typeEncodingOS << '_';
    else if (c == '*')
      typeEncodingOS << "ptr";
    else
      typeEncodingOS << c;
  }
  // Express the offset and end in terms of target word size.
  // This ensures that tests are able to use the stub struct name in target
  // independent manner.
  auto emitUnit = [&](const clang::CharUnits &unit) {
    typeEncodingOS << '_' << unit.getQuantity();
  };
  emitUnit(offset);
  emitUnit(end);
}

template <class T>
static std::string encodeTypeInfo(const T &abiTypeInfo,
                                  const ModuleDecl *moduleContext,
                                  PrimitiveTypeMapping &typeMapping) {
  std::string typeEncoding;
  llvm::raw_string_ostream typeEncodingOS(typeEncoding);

  ClangSyntaxPrinter(typeEncodingOS).printBaseName(moduleContext);
  abiTypeInfo.enumerateRecordMembers(
      [&](clang::CharUnits offset, clang::CharUnits end, Type t) {
        addABIRecordToTypeEncoding(typeEncodingOS, offset, end, t, typeMapping);
      });
  return std::move(typeEncodingOS.str());
}

template <class T>
static void printDirectReturnOrParamCType(
    const T &abiTypeInfo, Type valueType, const ModuleDecl *emittedModule,
    raw_ostream &os, raw_ostream &cPrologueOS,
    PrimitiveTypeMapping &typeMapping,
    SwiftToClangInteropContext &interopContext,
    llvm::function_ref<void()> prettifiedValuePrinter) {
  const bool isResultType =
      std::is_same<T, LoweredFunctionSignature::DirectResultType>::value;
  StringRef stubTypeName =
      isResultType ? "swift_interop_returnStub_" : "swift_interop_passStub_";

  std::string typeEncoding;
  llvm::raw_string_ostream typeEncodingOS(typeEncoding);
  typeEncodingOS << stubTypeName;
  ClangSyntaxPrinter(typeEncodingOS).printBaseName(emittedModule);

  unsigned Count = 0;
  clang::CharUnits lastOffset;
  abiTypeInfo.enumerateRecordMembers(
      [&](clang::CharUnits offset, clang::CharUnits end, Type t) {
        lastOffset = offset;
        ++Count;
        addABIRecordToTypeEncoding(typeEncodingOS, offset, end, t, typeMapping);
      });
  assert(Count > 0 && "missing return values");

  // FIXME: is this "prettyfying" logic sound for multiple return values?
  if (isKnownCType(valueType, typeMapping) ||
      (Count == 1 && lastOffset.isZero() && !valueType->hasTypeParameter() &&
       valueType->isAnyClassReferenceType())) {
    prettifiedValuePrinter();
    return;
  }

  os << "struct " << typeEncodingOS.str();
  llvm::SmallVector<std::pair<clang::CharUnits, clang::CharUnits>, 8> fields;
  auto printStub = [&](raw_ostream &os, StringRef stubName) {
    // Print out a C stub for this value type.
    os << "// Stub struct to be used to pass/return values to/from Swift "
          "functions.\n";
    os << "struct " << stubName << " {\n";
    abiTypeInfo.enumerateRecordMembers([&](clang::CharUnits offset,
                                           clang::CharUnits end, Type t) {
      auto info =
          typeMapping.getKnownCTypeInfo(t->getNominalOrBoundGenericNominal());
      os << "  " << info->name;
      if (info->canBeNullable)
        os << " _Nullable";
      os << " _" << (fields.size() + 1) << ";\n";
      fields.push_back(std::make_pair(offset, end));
    });
    os << "};\n\n";
    auto minimalStubName = stubName;
    minimalStubName.consume_front(stubTypeName);
    if (isResultType) {
      // Emit a stub that returns a value directly from swiftcc function.
      os << "static inline void swift_interop_returnDirect_" << minimalStubName;
      os << "(char * _Nonnull result, struct " << stubName << " value";
      os << ") __attribute__((always_inline)) {\n";
      for (size_t i = 0; i < fields.size(); ++i) {
        os << "  memcpy(result + " << fields[i].first.getQuantity() << ", "
           << "&value._" << (i + 1) << ", "
           << (fields[i].second - fields[i].first).getQuantity() << ");\n";
      }
    } else {
      // Emit a stub that is used to pass value type directly to swiftcc
      // function.
      os << "static inline struct " << stubName << " swift_interop_passDirect_"
         << minimalStubName;
      os << "(const char * _Nonnull value) __attribute__((always_inline)) {\n";
      os << "  struct " << stubName << " result;\n";
      for (size_t i = 0; i < fields.size(); ++i) {
        os << "  memcpy(&result._" << (i + 1) << ", value + "
           << fields[i].first.getQuantity() << ", "
           << (fields[i].second - fields[i].first).getQuantity() << ");\n";
      }
      os << "  return result;\n";
    }
    os << "}\n\n";
  };

  interopContext.runIfStubForDeclNotEmitted(typeEncodingOS.str(), [&]() {
    printStub(cPrologueOS, typeEncodingOS.str());
  });
}

ClangRepresentation DeclAndTypeClangFunctionPrinter::printFunctionSignature(
    const AbstractFunctionDecl *FD, const LoweredFunctionSignature &signature,
    StringRef name, Type resultTy, FunctionSignatureKind kind,
    FunctionSignatureModifiers modifiers) {
  // Print any template and requires clauses for the
  // C++ class context to which this C++ member will belong to.
  if (const auto *typeDecl = modifiers.qualifierContext) {
    assert(kind == FunctionSignatureKind::CxxInlineThunk);
    ClangSyntaxPrinter(os).printNominalTypeOutsideMemberDeclTemplateSpecifiers(
        typeDecl);
  }
  if (FD->isGeneric()) {
    auto Signature = FD->getGenericSignature().getCanonicalSignature();
    auto Requirements = Signature.getRequirements();
    // FIXME: Support generic requirements.
    if (!Requirements.empty())
      return ClangRepresentation::unsupported;
    // Print the template and requires clauses for this function.
    if (kind == FunctionSignatureKind::CxxInlineThunk)
      ClangSyntaxPrinter(os).printGenericSignature(Signature);
  }
  auto emittedModule = FD->getModuleContext();
  OutputLanguageMode outputLang = kind == FunctionSignatureKind::CFunctionProto
                                      ? OutputLanguageMode::ObjC
                                      : OutputLanguageMode::Cxx;
  // FIXME: Might need a PrintMultiPartType here.
  auto print =
      [&, this](Type ty, Optional<OptionalTypeKind> optionalKind,
                StringRef name, bool isInOutParam,
                CFunctionSignatureTypePrinterModifierDelegate delegate = {})
      -> ClangRepresentation {
    // FIXME: add support for noescape and PrintMultiPartType,
    // see DeclAndTypePrinter::print.
    CFunctionSignatureTypePrinter typePrinter(
        os, cPrologueOS, typeMapping, outputLang, interopContext, delegate,
        emittedModule, declPrinter);
    auto result = typePrinter.visit(ty, optionalKind, isInOutParam);

    if (!name.empty()) {
      os << ' ';
      ClangSyntaxPrinter(os).printIdentifier(name);
    }
    return result;
  };

  // Print any modifiers before the signature.
  if (modifiers.isStatic) {
    assert(!modifiers.isConst);
    os << "static ";
  }
  if (modifiers.isInline)
    os << "inline ";

  ClangRepresentation resultingRepresentation =
      ClangRepresentation::representable;

  // Print out the return type.
  if (kind == FunctionSignatureKind::CFunctionProto) {
    // First, verify that the C++ return type is representable.
    {
      OptionalTypeKind optKind;
      Type objTy;
      std::tie(objTy, optKind) =
          DeclAndTypePrinter::getObjectTypeAndOptionality(FD, resultTy);
      CFunctionSignatureTypePrinter typePrinter(
          llvm::nulls(), llvm::nulls(), typeMapping, OutputLanguageMode::Cxx,
          interopContext, CFunctionSignatureTypePrinterModifierDelegate(),
          emittedModule, declPrinter, FunctionSignatureTypeUse::ReturnType);
      if (resultingRepresentation
              .merge(typePrinter.visit(objTy, optKind, /*isInOutParam=*/false))
              .isUnsupported())
        return resultingRepresentation;
    }

    auto directResultType = signature.getDirectResultType();
    // FIXME: support direct + indirect results.
    if (directResultType && signature.getNumIndirectResultValues() > 0)
      return ClangRepresentation::unsupported;
    // FIXME: support multiple indirect results.
    if (signature.getNumIndirectResultValues() > 1)
      return ClangRepresentation::unsupported;

    if (!directResultType) {
      os << "void";
    } else {
      printDirectReturnOrParamCType(
          *directResultType, resultTy, emittedModule, os, cPrologueOS,
          typeMapping, interopContext, [&]() {
            OptionalTypeKind retKind;
            Type objTy;
            std::tie(objTy, retKind) =
                DeclAndTypePrinter::getObjectTypeAndOptionality(FD, resultTy);

            auto s = printClangFunctionReturnType(objTy, retKind, emittedModule,
                                                  outputLang);
            assert(!s.isUnsupported());
          });
    }
  } else {
    OptionalTypeKind retKind;
    Type objTy;
    std::tie(objTy, retKind) =
        DeclAndTypePrinter::getObjectTypeAndOptionality(FD, resultTy);
    if (resultingRepresentation
            .merge(printClangFunctionReturnType(objTy, retKind, emittedModule,
                                                outputLang))
            .isUnsupported())
      return resultingRepresentation;
  }

  os << ' ';
  if (const auto *typeDecl = modifiers.qualifierContext)
    ClangSyntaxPrinter(os).printNominalTypeQualifier(
        typeDecl, typeDecl->getModuleContext());
  ClangSyntaxPrinter(os).printIdentifier(name);
  os << '(';

  bool HasParams = false;

  if (kind == FunctionSignatureKind::CFunctionProto) {
    // First, verify that the C++ param types are representable.
    for (auto param : *FD->getParameters()) {
      OptionalTypeKind optKind;
      Type objTy;
      std::tie(objTy, optKind) =
          DeclAndTypePrinter::getObjectTypeAndOptionality(
              FD, param->getInterfaceType());
      CFunctionSignatureTypePrinter typePrinter(
          llvm::nulls(), llvm::nulls(), typeMapping, OutputLanguageMode::Cxx,
          interopContext, CFunctionSignatureTypePrinterModifierDelegate(),
          emittedModule, declPrinter, FunctionSignatureTypeUse::ParamType);
      if (resultingRepresentation
              .merge(typePrinter.visit(objTy, optKind,
                                       /*isInOutParam=*/param->isInOut()))
              .isUnsupported())
        return resultingRepresentation;
    }

    bool needsComma = false;
    auto emitNewParam = [&]() {
      if (needsComma)
        os << ", ";
      needsComma = true;
    };
    auto printParamName = [&](const ParamDecl &param) {
      std::string paramName =
          param.getName().empty() ? "" : param.getName().str().str();
      if (param.isSelfParameter())
        paramName = "_self";
      if (!paramName.empty()) {
        os << ' ';
        ClangSyntaxPrinter(os).printIdentifier(paramName);
      }
    };
    auto printParamCType = [&](const ParamDecl &param) {
      OptionalTypeKind optionalKind;
      Type ty;
      std::tie(ty, optionalKind) =
          DeclAndTypePrinter::getObjectTypeAndOptionality(
              &param, param.getInterfaceType());
      CFunctionSignatureTypePrinter typePrinter(
          os, cPrologueOS, typeMapping, outputLang, interopContext,
          CFunctionSignatureTypePrinterModifierDelegate(), emittedModule,
          declPrinter);
      auto s = typePrinter.visit(ty, optionalKind, param.isInOut());
      assert(!s.isUnsupported());
    };
    signature.visitParameterList(
        [&](const LoweredFunctionSignature::IndirectResultValue
                &indirectResult) {
          emitNewParam();
          if (indirectResult.hasSRet())
            os << "SWIFT_INDIRECT_RESULT ";
          // FIXME: it would be nice to print out the C struct type here.
          os << "void * _Nonnull";
        },
        [&](const LoweredFunctionSignature::DirectParameter &param) {
          emitNewParam();
          printDirectReturnOrParamCType(
              param, param.getParamDecl().getInterfaceType(), emittedModule, os,
              cPrologueOS, typeMapping, interopContext,
              [&]() { printParamCType(param.getParamDecl()); });
          printParamName(param.getParamDecl());
        },
        [&](const LoweredFunctionSignature::IndirectParameter &param) {
          emitNewParam();
          if (param.getParamDecl().isSelfParameter())
            os << "SWIFT_CONTEXT ";
          bool isConst =
              !param.getParamDecl().isInOut() &&
              !(param.getParamDecl().isSelfParameter() &&
                !param.getParamDecl().getInterfaceType()->hasTypeParameter() &&
                param.getParamDecl()
                    .getInterfaceType()
                    ->isAnyClassReferenceType());
          if (isConst)
            os << "const ";
          if (isKnownCType(param.getParamDecl().getInterfaceType(),
                           typeMapping) ||
              (!param.getParamDecl().getInterfaceType()->hasTypeParameter() &&
               param.getParamDecl()
                   .getInterfaceType()
                   ->isAnyClassReferenceType()))
            printParamCType(param.getParamDecl());
          else
            os << "void * _Nonnull";
          printParamName(param.getParamDecl());
        },
        [&](const LoweredFunctionSignature::GenericRequirementParameter
                &genericRequirementParam) {
          emitNewParam();
          os << "void * _Nonnull ";
          if (auto *proto = genericRequirementParam.getRequirement().Protocol)
            ClangSyntaxPrinter(os).printBaseName(proto);
        },
        [&](const LoweredFunctionSignature::MetadataSourceParameter
                &metadataSrcParam) {
          emitNewParam();
          os << "void * _Nonnull ";
        },
        [&](const LoweredFunctionSignature::ContextParameter &) {
          emitNewParam();
          os << "SWIFT_CONTEXT void * _Nonnull _ctx";
        },
        [&](const LoweredFunctionSignature::ErrorResultValue &) {
          emitNewParam();
          os << "SWIFT_ERROR_RESULT void * _Nullable * _Nullable _error";
        });
    if (needsComma == false)
      // Emit 'void' in an empty parameter list for C function declarations.
      os << "void";
    os << ')';
    return resultingRepresentation;
  }

  // Print out the C++ parameter types.
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
      // Always emit a named parameter for the C++ inline thunk to ensure it
      // can be referenced in the body.
      if (kind == FunctionSignatureKind::CxxInlineThunk && paramName.empty()) {
        llvm::raw_string_ostream os(paramName);
        os << "_" << paramIndex;
      }
      resultingRepresentation.merge(
          print(objTy, argKind, paramName, param->isInOut()));
      ++paramIndex;
    });
    if (resultingRepresentation.isUnsupported())
      return resultingRepresentation;
  }
  os << ')';
  if (modifiers.isConst)
    os << " const";
  return resultingRepresentation;
}

void DeclAndTypeClangFunctionPrinter::printTypeImplTypeSpecifier(
    Type type, const ModuleDecl *moduleContext) {
  CFunctionSignatureTypePrinterModifierDelegate delegate;
  delegate.mapValueTypeUseKind = [](ClangValueTypePrinter::TypeUseKind kind) {
    return ClangValueTypePrinter::TypeUseKind::CxxImplTypeName;
  };
  CFunctionSignatureTypePrinter typePrinter(
      os, cPrologueOS, typeMapping, OutputLanguageMode::Cxx, interopContext,
      delegate, moduleContext, declPrinter,
      FunctionSignatureTypeUse::TypeReference);
  auto result = typePrinter.visit(type, None, /*isInOut=*/false);
  assert(!result.isUnsupported());
}

void DeclAndTypeClangFunctionPrinter::printCxxToCFunctionParameterUse(
    Type type, StringRef name, const ModuleDecl *moduleContext, bool isInOut,
    bool isIndirect, std::string directTypeEncoding, bool isSelf) {
  auto namePrinter = [&]() { ClangSyntaxPrinter(os).printIdentifier(name); };
  if (!isKnownCxxType(type, typeMapping) &&
      !hasKnownOptionalNullableCxxMapping(type)) {
    if (type->is<GenericTypeParamType>()) {
      os << "swift::" << cxx_synthesis::getCxxImplNamespaceName()
         << "::getOpaquePointer(";
      namePrinter();
      os << ')';
      return;
    }

    if (auto *classDecl = type->getClassOrBoundGenericClass()) {
      ClangClassTypePrinter::printParameterCxxtoCUseScaffold(
          os, classDecl, moduleContext, namePrinter, isInOut);
      return;
    }

    if (auto *decl = type->getNominalOrBoundGenericNominal()) {
      if ((isa<StructDecl>(decl) || isa<EnumDecl>(decl))) {
        if (!directTypeEncoding.empty())
          os << cxx_synthesis::getCxxImplNamespaceName()
             << "::swift_interop_passDirect_" << directTypeEncoding << '(';
        ClangValueTypePrinter(os, cPrologueOS, interopContext)
            .printParameterCxxToCUseScaffold(
                moduleContext,
                [&]() { printTypeImplTypeSpecifier(type, moduleContext); },
                namePrinter, isSelf);
        if (!directTypeEncoding.empty())
          os << ')';
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

void DeclAndTypeClangFunctionPrinter::printCxxThunkBody(
    const AbstractFunctionDecl *FD, const LoweredFunctionSignature &signature,
    StringRef swiftSymbolName, const ModuleDecl *moduleContext, Type resultTy,
    const ParameterList *params, bool hasThrows,
    const AnyFunctionType *funcType) {
  if (hasThrows) {
    os << "  void* opaqueError = nullptr;\n";
    os << "  void* _ctx = nullptr;\n";
  }
  auto printCallToCFunc = [&](Optional<StringRef> additionalParam) {
    os << cxx_synthesis::getCxxImplNamespaceName() << "::" << swiftSymbolName
       << '(';

    bool needsComma = false;
    size_t paramIndex = 1;
    auto emitNewParam = [&]() {
      if (needsComma)
        os << ", ";
      needsComma = true;
    };
    auto printParamUse = [&](const ParamDecl &param, bool isIndirect,

                             std::string directTypeEncoding) {
      emitNewParam();
      std::string paramName;
      if (param.isSelfParameter()) {
        paramName = "*this";
      } else if (param.getName().empty()) {
        llvm::raw_string_ostream paramOS(paramName);
        paramOS << "_" << paramIndex;
      } else {
        paramName = param.getName().str().str();
      }
      ++paramIndex;
      printCxxToCFunctionParameterUse(param.getInterfaceType(), paramName,
                                      param.getModuleContext(), param.isInOut(),
                                      isIndirect, directTypeEncoding,
                                      param.isSelfParameter());
    };

    signature.visitParameterList(
        [&](const LoweredFunctionSignature::IndirectResultValue &) {
          emitNewParam();
          assert(additionalParam);
          os << *additionalParam;
          additionalParam = None;
        },
        [&](const LoweredFunctionSignature::DirectParameter &param) {
          printParamUse(param.getParamDecl(), /*isIndirect=*/false,
                        encodeTypeInfo(param, moduleContext, typeMapping));
        },
        [&](const LoweredFunctionSignature::IndirectParameter &param) {
          printParamUse(param.getParamDecl(), /*isIndirect=*/true,
                        /*directTypeEncoding=*/"");
        },
        [&](const LoweredFunctionSignature::GenericRequirementParameter
                &genericRequirementParam) {
          emitNewParam();
          auto genericRequirement = genericRequirementParam.getRequirement();
          // FIXME: Add protocol requirement support.
          assert(!genericRequirement.Protocol);
          if (auto *gtpt = genericRequirement.TypeParameter
                               ->getAs<GenericTypeParamType>()) {
            os << "swift::TypeMetadataTrait<";
            ClangSyntaxPrinter(os).printGenericTypeParamTypeName(gtpt);
            os << ">::getTypeMetadata()";
            return;
          }
          os << "ERROR";
        },
        [&](const LoweredFunctionSignature::MetadataSourceParameter
                &metadataSrcParam) {
          emitNewParam();
          os << "swift::TypeMetadataTrait<";
          CFunctionSignatureTypePrinterModifierDelegate delegate;
          CFunctionSignatureTypePrinter typePrinter(
              os, cPrologueOS, typeMapping, OutputLanguageMode::Cxx,
              interopContext, delegate, moduleContext, declPrinter,
              FunctionSignatureTypeUse::TypeReference);
          auto result = typePrinter.visit(metadataSrcParam.getType(), None,
                                          /*isInOut=*/false);
          assert(!result.isUnsupported());
          os << ">::getTypeMetadata()";
        },
        [&](const LoweredFunctionSignature::ContextParameter &) {
          emitNewParam();
          os << "_ctx";
        },
        [&](const LoweredFunctionSignature::ErrorResultValue &) {
          emitNewParam();
          os << "&opaqueError";
        });
    os << ')';
  };

  // Values types are returned either direcly in their C representation, or
  // indirectly by a pointer.
  if (!isKnownCxxType(resultTy, typeMapping) &&
      !hasKnownOptionalNullableCxxMapping(resultTy)) {
    if (const auto *gtpt = resultTy->getAs<GenericTypeParamType>()) {
      std::string returnAddress;
      llvm::raw_string_ostream ros(returnAddress);
      ros << "reinterpret_cast<void *>(&returnValue)";
      std::string resultTyName;
      {
        llvm::raw_string_ostream os(resultTyName);
        ClangSyntaxPrinter(os).printGenericTypeParamTypeName(gtpt);
      }

      os << "  if constexpr (std::is_base_of<::swift::"
         << cxx_synthesis::getCxxImplNamespaceName() << "::RefCountedClass, "
         << resultTyName << ">::value) {\n";
      os << "  void *returnValue;\n  ";
      printCallToCFunc(/*additionalParam=*/StringRef(ros.str()));
      os << ";\n";
      os << "  return ::swift::" << cxx_synthesis::getCxxImplNamespaceName()
         << "::implClassFor<" << resultTyName
         << ">::type::makeRetained(returnValue);\n";
      os << "  } else if constexpr (::swift::"
         << cxx_synthesis::getCxxImplNamespaceName() << "::isValueType<"
         << resultTyName << ">) {\n";
      os << "  return ::swift::" << cxx_synthesis::getCxxImplNamespaceName()
         << "::implClassFor<" << resultTyName
         << ">::type::returnNewValue([&](void * _Nonnull returnValue) {\n";
      printCallToCFunc(/*additionalParam=*/StringRef("returnValue"));
      os << ";\n  });\n";
      os << "  } else {\n";
      os << "  " << resultTyName << " returnValue;\n";
      printCallToCFunc(/*additionalParam=*/StringRef(ros.str()));
      os << ";\n  return returnValue;\n";
      os << "  }\n";
      return;
    }
    if (auto *classDecl = resultTy->getClassOrBoundGenericClass()) {
      ClangClassTypePrinter::printClassTypeReturnScaffold(
          os, classDecl, moduleContext,
          [&]() { printCallToCFunc(/*additionalParam=*/None); });
      return;
    }
    if (auto *decl = resultTy->getNominalOrBoundGenericNominal()) {
      ClangValueTypePrinter valueTypePrinter(os, cPrologueOS, interopContext);

      valueTypePrinter.printValueTypeReturnScaffold(
          decl, moduleContext,
          [&]() { printTypeImplTypeSpecifier(resultTy, moduleContext); },
          [&](StringRef resultPointerName) {
            if (auto directResultType = signature.getDirectResultType()) {
              std::string typeEncoding =
                  encodeTypeInfo(*directResultType, moduleContext, typeMapping);
              os << cxx_synthesis::getCxxImplNamespaceName()
                 << "::swift_interop_returnDirect_" << typeEncoding << '('
                 << resultPointerName << ", ";
              printCallToCFunc(None);
              os << ')';
            } else {
              printCallToCFunc(/*firstParam=*/resultPointerName);
            }
          });
      return;
    }
  }

  // Primitive values are returned directly without any conversions.
  // Assign the function return value to a variable if the function can throw.
  if (!resultTy->isVoid() && hasThrows)
    os << "  auto returnValue = ";
  // If the function doesn't have a return value just call it.
  else if (resultTy->isVoid() && hasThrows)
    os << "  ";
  // If the function can't throw just return its value result.
  else if (!hasThrows)
    os << "  return ";
  printCallToCFunc(/*additionalParam=*/None);
  os << ";\n";

  // Create the condition and the statement to throw an exception.
  if (hasThrows) {
    os << "  if (opaqueError != nullptr)\n";
    os << "    throw (swift::Error(opaqueError));\n";
  }

  // Return the function result value if it doesn't throw.
  if (!resultTy->isVoid() && hasThrows) {
    os << "\n";
    os << "return returnValue;\n";
  }
}

static StringRef getConstructorName(const AbstractFunctionDecl *FD) {
  auto name = cxx_translation::getNameForCxx(
      FD, cxx_translation::CustomNamesOnly_t::CustomNamesOnly);
  if (!name.empty()) {
    assert(name.startswith("init"));
    return name;
  }
  return "init";
}

void DeclAndTypeClangFunctionPrinter::printCxxMethod(
    const NominalTypeDecl *typeDeclContext, const AbstractFunctionDecl *FD,
    const LoweredFunctionSignature &signature, StringRef swiftSymbolName,
    Type resultTy, bool isDefinition) {
  bool isConstructor = isa<ConstructorDecl>(FD);
  os << "  ";

  FunctionSignatureModifiers modifiers;
  if (isDefinition)
    modifiers.qualifierContext = typeDeclContext;
  modifiers.isStatic = isConstructor && !isDefinition;
  modifiers.isInline = true;
  bool isMutating =
      isa<FuncDecl>(FD) ? cast<FuncDecl>(FD)->isMutating() : false;
  modifiers.isConst =
      !isa<ClassDecl>(typeDeclContext) && !isMutating && !isConstructor;
  auto result = printFunctionSignature(
      FD, signature,
      isConstructor ? getConstructorName(FD)
                    : cxx_translation::getNameForCxx(FD),
      resultTy, FunctionSignatureKind::CxxInlineThunk, modifiers);
  assert(!result.isUnsupported() && "C signature should be unsupported too");

  if (!isDefinition) {
    os << ";\n";
    return;
  }

  os << " {\n";
  // FIXME: should it be objTy for resultTy?
  printCxxThunkBody(FD, signature, swiftSymbolName, FD->getModuleContext(),
                    resultTy, FD->getParameters(), FD->hasThrows(),
                    FD->getInterfaceType()->castTo<AnyFunctionType>());
  os << "  }\n";
}

/// Returns true if the given property name like `isEmpty` can be remapped
/// directly to a C++ method.
static bool canRemapBoolPropertyNameDirectly(StringRef name) {
  auto startsWithAndLonger = [&](StringRef prefix) -> bool {
    return name.startswith(prefix) && name.size() > prefix.size();
  };
  return startsWithAndLonger("is") || startsWithAndLonger("has");
}

static std::string remapPropertyName(const AccessorDecl *accessor,
                                     Type resultTy) {
  // For a getter or setter, go through the variable or subscript decl.
  StringRef propertyName =
      cxx_translation::getNameForCxx(accessor->getStorage());

  // Boolean property getters can be remapped directly in certain cases.
  if (accessor->isGetter() && resultTy->isBool() &&
      canRemapBoolPropertyNameDirectly(propertyName)) {
    return propertyName.str();
  }

  std::string name;
  llvm::raw_string_ostream nameOS(name);
  nameOS << (accessor->isSetter() ? "set" : "get")
         << char(std::toupper(propertyName[0])) << propertyName.drop_front();
  nameOS.flush();
  return name;
}

void DeclAndTypeClangFunctionPrinter::printCxxPropertyAccessorMethod(
    const NominalTypeDecl *typeDeclContext, const AccessorDecl *accessor,
    const LoweredFunctionSignature &signature, StringRef swiftSymbolName,
    Type resultTy, bool isDefinition) {
  assert(accessor->isSetter() || accessor->getParameters()->size() == 0);
  os << "  ";

  FunctionSignatureModifiers modifiers;
  if (isDefinition)
    modifiers.qualifierContext = typeDeclContext;
  modifiers.isInline = true;
  modifiers.isConst = accessor->isGetter() && !isa<ClassDecl>(typeDeclContext);
  auto result = printFunctionSignature(
      accessor, signature, remapPropertyName(accessor, resultTy), resultTy,
      FunctionSignatureKind::CxxInlineThunk, modifiers);
  assert(!result.isUnsupported() && "C signature should be unsupported too!");
  if (!isDefinition) {
    os << ";\n";
    return;
  }
  os << " {\n";
  // FIXME: should it be objTy for resultTy?
  printCxxThunkBody(accessor, signature, swiftSymbolName,
                    accessor->getModuleContext(), resultTy,
                    accessor->getParameters());
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

void DeclAndTypeClangFunctionPrinter::printCustomCxxFunction(
    const SmallVector<Type> &neededTypes, PrinterTy retTypeAndNamePrinter,
    PrinterTy paramPrinter, bool isConstFunc, PrinterTy bodyPrinter,
    ModuleDecl *emittedModule, raw_ostream &outOfLineOS) {
  llvm::MapVector<Type, std::string> types;

  for (auto &type : neededTypes) {
    std::string typeStr;
    llvm::raw_string_ostream typeOS(typeStr);
    OptionalTypeKind optKind;
    Type objectType;
    std::tie(objectType, optKind) =
        DeclAndTypePrinter::getObjectTypeAndOptionality(
            type->getNominalOrBoundGenericNominal(), type);

    // Use FunctionSignatureTypeUse::ReturnType to avoid printing extra const or
    // references
    CFunctionSignatureTypePrinter typePrinter(
        typeOS, cPrologueOS, typeMapping, OutputLanguageMode::Cxx,
        interopContext, CFunctionSignatureTypePrinterModifierDelegate(),
        emittedModule, declPrinter, FunctionSignatureTypeUse::ReturnType);
    typePrinter.visit(objectType, optKind, /* isInOutParam */ false);

    types.insert({type, typeStr});
  }

  retTypeAndNamePrinter(types);
  os << '(';
  outOfLineOS << '(';
  paramPrinter(types);
  os << ')';
  outOfLineOS << ')';
  if (isConstFunc) {
    os << " const;\n";
    outOfLineOS << " const";
  }
  outOfLineOS << " {\n";
  bodyPrinter(types);
  outOfLineOS << "}\n";
}
