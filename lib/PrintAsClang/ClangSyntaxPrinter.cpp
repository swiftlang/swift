//===--- ClangSyntaxPrinter.cpp - Printer for C and C++ code ----*- C++ -*-===//
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

#include "ClangSyntaxPrinter.h"
#include "swift/ABI/MetadataValues.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/SwiftNameTranslation.h"

using namespace swift;
using namespace cxx_synthesis;

StringRef cxx_synthesis::getCxxSwiftNamespaceName() { return "swift"; }

StringRef cxx_synthesis::getCxxImplNamespaceName() { return "_impl"; }

StringRef cxx_synthesis::getCxxOpaqueStorageClassName() {
  return "OpaqueStorage";
}

bool ClangSyntaxPrinter::isClangKeyword(StringRef name) {
  static const llvm::DenseSet<StringRef> keywords = [] {
    llvm::DenseSet<StringRef> set;
    // FIXME: clang::IdentifierInfo /nearly/ has the API we need to do this
    // in a more principled way, but not quite.
#define KEYWORD(SPELLING, FLAGS) set.insert(#SPELLING);
#define CXX_KEYWORD_OPERATOR(SPELLING, TOK) set.insert(#SPELLING);
#include "clang/Basic/TokenKinds.def"
    return set;
  }();

  return keywords.contains(name);
}

bool ClangSyntaxPrinter::isClangKeyword(Identifier name) {
  if (name.empty())
    return false;
  return ClangSyntaxPrinter::isClangKeyword(name.str());
}

void ClangSyntaxPrinter::printIdentifier(StringRef name) {
  os << name;
  if (ClangSyntaxPrinter::isClangKeyword(name))
    os << '_';
}

void ClangSyntaxPrinter::printBaseName(const ValueDecl *decl) {
  assert(decl->getName().isSimpleName());
  printIdentifier(cxx_translation::getNameForCxx(decl));
}

void ClangSyntaxPrinter::printModuleNameCPrefix(const ModuleDecl &mod) {
  os << mod.getName().str() << '_';
}

void ClangSyntaxPrinter::printModuleNamespaceQualifiersIfNeeded(
    const ModuleDecl *referencedModule, const ModuleDecl *currentContext) {
  if (referencedModule == currentContext)
    return;
  printBaseName(referencedModule);
  os << "::";
}

bool ClangSyntaxPrinter::printNominalTypeOutsideMemberDeclTemplateSpecifiers(
    const NominalTypeDecl *typeDecl) {
  // FIXME: Full qualifiers for nested types?
  if (!typeDecl->isGeneric())
    return true;
  printGenericSignature(
      typeDecl->getGenericSignature().getCanonicalSignature());
  return false;
}

void ClangSyntaxPrinter::printNominalTypeReference(
    const NominalTypeDecl *typeDecl, const ModuleDecl *moduleContext) {
  printModuleNamespaceQualifiersIfNeeded(typeDecl->getModuleContext(),
                                         moduleContext);
  // FIXME: Full qualifiers for nested types?
  ClangSyntaxPrinter(os).printBaseName(typeDecl);
  if (typeDecl->isGeneric())
    printGenericSignatureParams(
        typeDecl->getGenericSignature().getCanonicalSignature());
}

void ClangSyntaxPrinter::printNominalTypeQualifier(
    const NominalTypeDecl *typeDecl, const ModuleDecl *moduleContext) {
  printNominalTypeReference(typeDecl, moduleContext);
  os << "::";
}

/// Print a C++ namespace declaration with the give name and body.
void ClangSyntaxPrinter::printNamespace(
    llvm::function_ref<void(raw_ostream &OS)> namePrinter,
    llvm::function_ref<void(raw_ostream &OS)> bodyPrinter) const {
  os << "namespace ";
  namePrinter(os);
  os << " {\n\n";
  bodyPrinter(os);
  os << "\n} // namespace ";
  namePrinter(os);
  os << "\n\n";
}

void ClangSyntaxPrinter::printNamespace(
    StringRef name,
    llvm::function_ref<void(raw_ostream &OS)> bodyPrinter) const {
  printNamespace([&](raw_ostream &os) { os << name; }, bodyPrinter);
}

void ClangSyntaxPrinter::printExternC(
    llvm::function_ref<void(raw_ostream &OS)> bodyPrinter) const {
  os << "#ifdef __cplusplus\n";
  os << "extern \"C\" {\n";
  os << "#endif\n\n";
  bodyPrinter(os);
  os << "\n#ifdef __cplusplus\n";
  os << "}\n";
  os << "#endif\n";
}

void ClangSyntaxPrinter::printObjCBlock(
    llvm::function_ref<void(raw_ostream &OS)> bodyPrinter) const {
  os << "#if defined(__OBJC__)\n";
  bodyPrinter(os);
  os << "\n#endif\n";
}

void ClangSyntaxPrinter::printSwiftImplQualifier() const {
  os << "swift::" << cxx_synthesis::getCxxImplNamespaceName() << "::";
}

void ClangSyntaxPrinter::printInlineForThunk() const {
  // FIXME: make a macro and add 'nodebug', and
  // migrate all other 'inline' uses.
  os << "inline __attribute__((always_inline)) ";
}

void ClangSyntaxPrinter::printNullability(
    Optional<OptionalTypeKind> kind, NullabilityPrintKind printKind) const {
  if (!kind)
    return;

  switch (printKind) {
  case NullabilityPrintKind::ContextSensitive:
    switch (*kind) {
    case OTK_None:
      os << "nonnull";
      break;
    case OTK_Optional:
      os << "nullable";
      break;
    case OTK_ImplicitlyUnwrappedOptional:
      os << "null_unspecified";
      break;
    }
    break;
  case NullabilityPrintKind::After:
    os << ' ';
    LLVM_FALLTHROUGH;
  case NullabilityPrintKind::Before:
    switch (*kind) {
    case OTK_None:
      os << "_Nonnull";
      break;
    case OTK_Optional:
      os << "_Nullable";
      break;
    case OTK_ImplicitlyUnwrappedOptional:
      os << "_Null_unspecified";
      break;
    }
    break;
  }

  if (printKind != NullabilityPrintKind::After)
    os << ' ';
}

void ClangSyntaxPrinter::printSwiftTypeMetadataAccessFunctionCall(
    StringRef name, ArrayRef<GenericRequirement> requirements) {
  os << name << "(0";
  printGenericRequirementsInstantiantions(requirements, LeadingTrivia::Comma);
  os << ')';
}

void ClangSyntaxPrinter::printValueWitnessTableAccessSequenceFromTypeMetadata(
    StringRef metadataVariable, StringRef vwTableVariable, int indent) {
  os << std::string(indent, ' ');
  os << "auto *vwTableAddr = ";
  os << "reinterpret_cast<";
  printSwiftImplQualifier();
  os << "ValueWitnessTable **>(" << metadataVariable << "._0) - 1;\n";
  os << "#ifdef __arm64e__\n";
  os << std::string(indent, ' ');
  os << "auto *" << vwTableVariable << " = ";
  os << "reinterpret_cast<";
  printSwiftImplQualifier();
  os << "ValueWitnessTable *>(ptrauth_auth_data(";
  os << "reinterpret_cast<void *>(*vwTableAddr), "
        "ptrauth_key_process_independent_data, ";
  os << "ptrauth_blend_discriminator(vwTableAddr, "
     << SpecialPointerAuthDiscriminators::ValueWitnessTable << ")));\n";
  os << "#else\n";
  os << std::string(indent, ' ');
  os << "auto *" << vwTableVariable << " = *vwTableAddr;\n";
  os << "#endif\n";
}

void ClangSyntaxPrinter::printCTypeMetadataTypeFunction(
    const NominalTypeDecl *typeDecl, StringRef typeMetadataFuncName,
    llvm::ArrayRef<GenericRequirement> genericRequirements) {
  // FIXME: Support generic requirements > 3.
  if (!genericRequirements.empty())
    os << "static_assert(" << genericRequirements.size()
       << " <= " << NumDirectGenericTypeMetadataAccessFunctionArgs
       << ", \"unsupported generic requirement list for metadata func\");\n";
  os << "// Type metadata accessor for " << typeDecl->getNameStr() << "\n";
  os << "SWIFT_EXTERN ";
  printSwiftImplQualifier();
  os << "MetadataResponseTy " << typeMetadataFuncName << '(';
  printSwiftImplQualifier();
  os << "MetadataRequestTy";
  if (!genericRequirements.empty())
    os << ", ";
  llvm::interleaveComma(genericRequirements, os,
                        [&](const GenericRequirement &) {
                          // FIXME: Print parameter name.
                          os << "void * _Nonnull";
                        });
  os << ')';
  os << " SWIFT_NOEXCEPT SWIFT_CALL;\n\n";
}

void ClangSyntaxPrinter::printGenericTypeParamTypeName(
    const GenericTypeParamType *gtpt) {
  os << "T_" << gtpt->getDepth() << '_' << gtpt->getIndex();
}

void ClangSyntaxPrinter::printGenericSignature(
    const CanGenericSignature &signature) {
  os << "template<";
  llvm::interleaveComma(
      signature.getGenericParams(), os,
      [&](const CanTypeWrapper<GenericTypeParamType> &genericParamType) {
        os << "class ";
        printGenericTypeParamTypeName(genericParamType);
      });
  os << ">\n";
  os << "requires ";
  llvm::interleave(
      signature.getGenericParams(), os,
      [&](const CanTypeWrapper<GenericTypeParamType> &genericParamType) {
        os << "swift::isUsableInGenericContext<";
        printGenericTypeParamTypeName(genericParamType);
        os << ">";
      },
      " && ");
  os << "\n";
}

void ClangSyntaxPrinter::printGenericSignatureParams(
    const CanGenericSignature &signature) {
  os << '<';
  llvm::interleaveComma(
      signature.getGenericParams(), os,
      [&](const CanTypeWrapper<GenericTypeParamType> &genericParamType) {
        printGenericTypeParamTypeName(genericParamType);
      });
  os << '>';
}

void ClangSyntaxPrinter::printGenericRequirementInstantiantion(
    const GenericRequirement &requirement) {
  assert(!requirement.Protocol && "protocol requirements not supported yet!");
  auto *gtpt = requirement.TypeParameter->getAs<GenericTypeParamType>();
  assert(gtpt && "unexpected generic param type");
  os << "swift::TypeMetadataTrait<";
  printGenericTypeParamTypeName(gtpt);
  os << ">::getTypeMetadata()";
}

void ClangSyntaxPrinter::printGenericRequirementsInstantiantions(
    ArrayRef<GenericRequirement> requirements, LeadingTrivia leadingTrivia) {
  if (leadingTrivia == LeadingTrivia::Comma && !requirements.empty())
    os << ", ";
  llvm::interleaveComma(requirements, os,
                        [&](const GenericRequirement &requirement) {
                          printGenericRequirementInstantiantion(requirement);
                        });
}

void ClangSyntaxPrinter::printPrimaryCxxTypeName(
    const NominalTypeDecl *type, const ModuleDecl *moduleContext) {
  printModuleNamespaceQualifiersIfNeeded(type->getModuleContext(),
                                         moduleContext);
  // FIXME: Print class qualifiers for nested class references.
  printBaseName(type);
}
