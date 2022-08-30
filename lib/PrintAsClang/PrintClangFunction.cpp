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
  const TypeDecl *typeDecl;
  if (auto *bgt = dyn_cast<BoundGenericStructType>(
          t->isOptional() ? t->getOptionalObjectType()->getDesugaredType()
                          : t->getDesugaredType())) {
    return bgt->isUnsafePointer() || bgt->isUnsafeMutablePointer();
  }

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

bool isGenericType(Type t) { return t->hasTypeParameter(); }

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
  /// Prefix the indirect value type / class type param being printed in C mode.
  Optional<llvm::function_ref<void(raw_ostream &)>>
      prefixIndirectlyPassedParamTypeInC = None;
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
      if (modifiersDelegate.prefixIndirectlyPassedParamTypeInC)
        (*modifiersDelegate.prefixIndirectlyPassedParamTypeInC)(os);
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
    return visitValueType(ET->getNominalOrBoundGenericNominal(), ET,
                          optionalKind, isInOutParam);
  }

  ClangRepresentation visitStructType(StructType *ST,
                                      Optional<OptionalTypeKind> optionalKind,
                                      bool isInOutParam) {
    return visitValueType(ST->getNominalOrBoundGenericNominal(), ST,
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

  ClangRepresentation visitValueType(const NominalTypeDecl *decl,
                                     NominalType *NT,
                                     Optional<OptionalTypeKind> optionalKind,
                                     bool isInOutParam,
                                     ArrayRef<Type> genericArgs = {}) {
    if (NT)
      assert(isa<StructType>(NT) || isa<EnumType>(NT));
    assert(isa<StructDecl>(decl) || isa<EnumDecl>(decl));

    // Handle known type names.
    if (printIfKnownSimpleType(decl, optionalKind, isInOutParam))
      return ClangRepresentation::representable;
    if (!declPrinter.shouldInclude(decl))
      return ClangRepresentation::unsupported; // FIXME: propagate why it's not
                                               // exposed.
    // FIXME: Handle optional structures.
    if (typeUseKind == FunctionSignatureTypeUse::ParamType) {
      if (languageMode != OutputLanguageMode::Cxx && !genericArgs.empty()) {
        // FIXME: what about concrete generic type.
        // FIXME: inout.
        os << "const void * _Nonnull";
        return ClangRepresentation::representable;
      }
      if (languageMode != OutputLanguageMode::Cxx &&
          (decl->isResilient() ||
           (NT && interopContext.getIrABIDetails().shouldPassIndirectly(NT)))) {
        if (modifiersDelegate.prefixIndirectlyPassedParamTypeInC)
          (*modifiersDelegate.prefixIndirectlyPassedParamTypeInC)(os);
        // FIXME: it would be nice to print out the C struct type here.
        if (isInOutParam) {
          os << "void * _Nonnull";
        } else {
          os << "const void * _Nonnull";
        }

      } else if (languageMode != OutputLanguageMode::Cxx) {
        ClangValueTypePrinter(os, cPrologueOS, typeMapping, interopContext)
            .printValueTypeParameterType(decl, languageMode, moduleContext,
                                         isInOutParam);
      } else {
        if (!isInOutParam) {
          os << "const ";
        }
        ClangSyntaxPrinter(os).printPrimaryCxxTypeName(decl, moduleContext);
        auto result = visitGenericArgs(genericArgs);
        os << '&';
        return result;
      }
    } else {
      ClangValueTypePrinter(os, cPrologueOS, typeMapping, interopContext)
          .printValueTypeReturnType(
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
    return visitValueType(BGT->getDecl(), nullptr, optionalKind, isInOutParam,
                          BGT->getGenericArgs());
  }

  ClangRepresentation
  visitGenericTypeParamType(GenericTypeParamType *genericTpt,
                            Optional<OptionalTypeKind> optionalKind,
                            bool isInOutParam) {
    // FIXME: handle optionalKind.
    if (typeUseKind == FunctionSignatureTypeUse::ReturnType ||
        typeUseKind == FunctionSignatureTypeUse::TypeReference) {
      // generic is always returned indirectly in C signature.
      assert(languageMode == OutputLanguageMode::Cxx);
      os << genericTpt->getName();
      return ClangRepresentation::representable;
    }
    if (!isInOutParam)
      os << "const ";
    if (languageMode == OutputLanguageMode::Cxx) {
      // Pass a reference to a template type.
      os << genericTpt->getName();
      os << " &";
      return ClangRepresentation::representable;
    }
    // Pass an opaque param in C mode.
    os << "void * _Nonnull";
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

ClangRepresentation DeclAndTypeClangFunctionPrinter::printFunctionSignature(
    const AbstractFunctionDecl *FD, StringRef name, Type resultTy,
    FunctionSignatureKind kind, ArrayRef<AdditionalParam> additionalParams,
    FunctionSignatureModifiers modifiers) {
  if (FD->isGeneric()) {
    auto Signature = FD->getGenericSignature();
    auto Requirements = Signature.getRequirements();
    // FIXME: Support generic requirements.
    if (!Requirements.empty())
      return ClangRepresentation::unsupported;
    if (kind == FunctionSignatureKind::CxxInlineThunk) {
      os << "template<";
      llvm::interleaveComma(FD->getGenericParams()->getParams(), os,
                            [&](const GenericTypeParamDecl *genericParam) {
                              os << "class ";
                              ClangSyntaxPrinter(os).printBaseName(
                                  genericParam);
                            });
      os << ">\n";
      os << "requires ";
      llvm::interleave(
          FD->getGenericParams()->getParams(), os,
          [&](const GenericTypeParamDecl *genericParam) {
            os << "swift::isUsableInGenericContext<";
            ClangSyntaxPrinter(os).printBaseName(genericParam);
            os << ">";
          },
          " && ");
      os << "\n";
    }
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
  bool isIndirectReturnType =
      kind == FunctionSignatureKind::CFunctionProto &&
      !isKnownCType(resultTy, typeMapping) &&
      ((isResilientType(resultTy) && !resultTy->isAnyClassReferenceType()) ||
       isGenericType(resultTy) ||
       interopContext.getIrABIDetails().shouldReturnIndirectly(resultTy));
  if (!isIndirectReturnType) {
    OptionalTypeKind retKind;
    Type objTy;
    std::tie(objTy, retKind) =
        DeclAndTypePrinter::getObjectTypeAndOptionality(FD, resultTy);
    if (resultingRepresentation
            .merge(printClangFunctionReturnType(objTy, retKind, emittedModule,
                                                outputLang))
            .isUnsupported())
      return resultingRepresentation;
  } else {
    // FIXME: also try to figure out if indirect is representable using a type
    // visitor?
    if (const auto *NT = resultTy->getNominalOrBoundGenericNominal()) {
      if (!declPrinter.shouldInclude(NT))
        return ClangRepresentation::unsupported;
    }
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
      resultingRepresentation.merge(
          print(objTy, argKind, paramName, param->isInOut()));
      ++paramIndex;
    });
    if (resultingRepresentation.isUnsupported())
      return resultingRepresentation;
  }
  if (additionalParams.size()) {
    assert(kind == FunctionSignatureKind::CFunctionProto);
    if (HasParams)
      os << ", ";
    HasParams = true;
    interleaveComma(additionalParams, os, [&](const AdditionalParam &param) {
      if (param.role == AdditionalParam::Role::Self) {
        CFunctionSignatureTypePrinterModifierDelegate delegate;
        delegate.prefixIndirectlyPassedParamTypeInC = [](raw_ostream &os) {
          os << "SWIFT_CONTEXT ";
        };
        if (FD->hasThrows())
          os << "SWIFT_CONTEXT ";
        if (param.isIndirect) {
          (*delegate.prefixIndirectlyPassedParamTypeInC)(os);
          os << "void * _Nonnull _self";
        } else {
          resultingRepresentation.merge(
              print(param.type, OptionalTypeKind::OTK_None, "_self",
                    /*isInOut*/ false, delegate));
        }
      } else if (param.role ==  AdditionalParam::Role::Error) {
        os << "SWIFT_ERROR_RESULT ";
        os << "void ** _error";
      } else if (param.role == AdditionalParam::Role::GenericRequirement) {
        os << "void * _Nonnull ";
        if (param.genericRequirement->Protocol)
          ClangSyntaxPrinter(os).printBaseName(
              param.genericRequirement->Protocol);
      }
    });
  }
  if (kind == FunctionSignatureKind::CFunctionProto && !HasParams) {
    // Emit 'void' in an empty parameter list for C function declarations.
    os << "void";
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
    bool isIndirect, llvm::Optional<AdditionalParam::Role> paramRole) {
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
        ClangValueTypePrinter(os, cPrologueOS, typeMapping, interopContext)
            .printParameterCxxToCUseScaffold(
                isIndirect || decl->isResilient() || isGenericType(type) ||
                    interopContext.getIrABIDetails().shouldPassIndirectly(type),
                decl, moduleContext,
                [&]() { printTypeImplTypeSpecifier(type, moduleContext); },
                namePrinter, isInOut,
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
  printCxxToCFunctionParameterUse(param->getInterfaceType(), name,
                                  param->getModuleContext(), param->isInOut());
}

void DeclAndTypeClangFunctionPrinter::printCxxThunkBody(
    StringRef swiftSymbolName, const ModuleDecl *moduleContext, Type resultTy,
    const ParameterList *params, ArrayRef<AdditionalParam> additionalParams,
    bool hasThrows, const AnyFunctionType *funcType) {
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
        if (param.role == AdditionalParam::Role::GenericRequirement) {
          auto genericRequirement = *param.genericRequirement;
          // FIXME: Add protocol requirement support.
          assert(!genericRequirement.Protocol);
          if (auto *gtpt = genericRequirement.TypeParameter
                               ->getAs<GenericTypeParamType>()) {
            assert(funcType);
            auto *gft = dyn_cast<GenericFunctionType>(funcType);
            if (gtpt->getDepth() == 0) {
              os << "swift::getTypeMetadata<"
                 << gft->getGenericParams()[gtpt->getIndex()]->getName()
                 << ">()";
              return;
            }
          }
          os << "ERROR";
          return;
        }
        if (param.role == AdditionalParam::Role::Self && !hasThrows)
          printCxxToCFunctionParameterUse(
              param.type, "*this", moduleContext, /*isInOut=*/false,
              /*isIndirect=*/param.isIndirect, param.role);
        else if (param.role == AdditionalParam::Role::Self && hasThrows)
          printCxxToCFunctionParameterUse(
              param.type, "self", moduleContext, /*isInOut=*/false,
              /*isIndirect=*/param.isIndirect, param.role);
        else if (param.role == AdditionalParam::Role::Error && hasThrows)
          printCxxToCFunctionParameterUse(
              param.type, "&opaqueError", moduleContext, /*isInOut=*/false,
              /*isIndirect=*/param.isIndirect, param.role);
      });
    }

    os << ')';
  };

  // Values types are returned either direcly in their C representation, or
  // indirectly by a pointer.
  if (!isKnownCxxType(resultTy, typeMapping) &&
      !hasKnownOptionalNullableCxxMapping(resultTy)) {
    if (resultTy->is<GenericTypeParamType>()) {
      std::string returnAddress;
      llvm::raw_string_ostream ros(returnAddress);
      ros << "reinterpret_cast<void *>(&returnValue)";
      StringRef resultTyName = "T"; // FIXME

      os << "  if constexpr (std::is_base_of<::swift::"
         << cxx_synthesis::getCxxImplNamespaceName()
         << "::RefCountedClass, T>::value) {\n";
      os << "  void *returnValue;\n  ";
      printCallToCFunc(/*additionalParam=*/StringRef(ros.str()));
      os << ";\n";
      os << "  return ::swift::" << cxx_synthesis::getCxxImplNamespaceName()
         << "::implClassFor<T>::type::makeRetained(returnValue);\n";
      os << "  } else if constexpr (::swift::"
         << cxx_synthesis::getCxxImplNamespaceName() << "::isValueType<"
         << resultTyName << ">) {\n";
      os << "  return ::swift::" << cxx_synthesis::getCxxImplNamespaceName()
         << "::implClassFor<" << resultTyName
         << ">::type::returnNewValue([&](void * _Nonnull returnValue) {\n";
      printCallToCFunc(/*additionalParam=*/StringRef("returnValue"));
      os << ";\n  });\n";
      os << "  } else {\n";
      os << "  T returnValue;\n";
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
      if ((isa<StructDecl>(decl) || isa<EnumDecl>(decl))) {
        bool isIndirect =
            decl->isResilient() || isGenericType(resultTy) ||
            interopContext.getIrABIDetails().shouldReturnIndirectly(resultTy);
        ClangValueTypePrinter valueTypePrinter(os, cPrologueOS, typeMapping,
                                               interopContext);
        if (isIndirect) {
          valueTypePrinter.printValueTypeIndirectReturnScaffold(
              decl, moduleContext,
              [&]() { printTypeImplTypeSpecifier(resultTy, moduleContext); },
              [&](StringRef returnParam) {
                printCallToCFunc(/*additionalParam=*/returnParam);
              });
        } else {
          valueTypePrinter.printValueTypeDirectReturnScaffold(
              decl, moduleContext,
              [&]() { printCallToCFunc(/*additionalParam=*/None); });
        }
        return;
      }
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
    os << "    throw (swift::_impl::NaiveException(\"Exception\"));\n";
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
    StringRef swiftSymbolName, Type resultTy, bool isDefinition,
    ArrayRef<AdditionalParam> additionalParams) {
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
      FD,
      isConstructor ? getConstructorName(FD)
                    : cxx_translation::getNameForCxx(FD),
      resultTy, FunctionSignatureKind::CxxInlineThunk, {}, modifiers);
  assert(!result.isUnsupported() && "C signature should be unsupported too");

  if (!isDefinition) {
    os << ";\n";
    return;
  }

  os << " {\n";
  // FIXME: should it be objTy for resultTy?
  printCxxThunkBody(swiftSymbolName, FD->getModuleContext(), resultTy,
                    FD->getParameters(), additionalParams, FD->hasThrows(),
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
    StringRef swiftSymbolName, Type resultTy, bool isDefinition) {
  assert(accessor->isSetter() || accessor->getParameters()->size() == 0);
  os << "  ";

  FunctionSignatureModifiers modifiers;
  if (isDefinition)
    modifiers.qualifierContext = typeDeclContext;
  modifiers.isInline = true;
  modifiers.isConst = accessor->isGetter() && !isa<ClassDecl>(typeDeclContext);
  auto result = printFunctionSignature(
      accessor, remapPropertyName(accessor, resultTy), resultTy,
      FunctionSignatureKind::CxxInlineThunk, {}, modifiers);
  assert(!result.isUnsupported() && "C signature should be unsupported too!");
  if (!isDefinition) {
    os << ";\n";
    return;
  }
  os << " {\n";
  // FIXME: should it be objTy for resultTy?
  printCxxThunkBody(swiftSymbolName, accessor->getModuleContext(), resultTy,
                    accessor->getParameters(),
                    {AdditionalParam{AdditionalParam::Role::Self,
                                     typeDeclContext->getDeclaredType(),
                                     /*isIndirect=*/accessor->isSetter()}});
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
