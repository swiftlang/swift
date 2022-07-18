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
#include "swift/AST/Module.h"

using namespace swift;
using namespace cxx_synthesis;

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
  printIdentifier(decl->getBaseIdentifier().str());
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

void ClangSyntaxPrinter::printSwiftImplQualifier() const {
  os << "swift::" << cxx_synthesis::getCxxImplNamespaceName() << "::";
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
    StringRef name) {
  os << name << "(0)";
}

void ClangSyntaxPrinter::printValueWitnessTableAccessSequenceFromTypeMetadata(
    StringRef metadataVariable) {
  os << "    auto *vwTableAddr = ";
  os << "reinterpret_cast<";
  printSwiftImplQualifier();
  os << "ValueWitnessTable **>(" << metadataVariable << "._0) - 1;\n";
  os << "#ifdef __arm64e__\n";
  os << "    auto *vwTable = ";
  os << "reinterpret_cast<";
  printSwiftImplQualifier();
  os << "ValueWitnessTable *>(ptrauth_auth_data(";
  os << "reinterpret_cast<void *>(*vwTableAddr), "
        "ptrauth_key_process_independent_data, ";
  os << "ptrauth_blend_discriminator(vwTableAddr, "
     << SpecialPointerAuthDiscriminators::ValueWitnessTable << ")));\n";
  os << "#else\n";
  os << "    auto *vwTable = *vwTableAddr;\n";
  os << "#endif\n";
}
