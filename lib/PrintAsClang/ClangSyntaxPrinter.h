//===--- ClangSyntaxPrinter.h - Printer for C and C++ code ------*- C++ -*-===//
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

#ifndef SWIFT_PRINTASCLANG_CLANGSYNTAXPRINTER_H
#define SWIFT_PRINTASCLANG_CLANGSYNTAXPRINTER_H

#include "swift/AST/GenericRequirement.h"
#include "swift/Basic/LLVM.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {

class CanGenericSignature;
class GenericTypeParamType;
class ModuleDecl;
class NominalTypeDecl;

namespace cxx_synthesis {

/// Return the name of the namespace for things exported from Swift stdlib
StringRef getCxxSwiftNamespaceName();

/// Return the name of the implementation namespace that is used to hide
/// declarations from the namespace that corresponds to the imported Swift
/// module in C++.
StringRef getCxxImplNamespaceName();

/// Return the name of the C++ class inside of `swift::_impl`
/// namespace that holds an opaque value, like a resilient struct.
StringRef getCxxOpaqueStorageClassName();

} // end namespace cxx_synthesis

class ClangSyntaxPrinter {
public:
  enum class LeadingTrivia { None, Comma };

  ClangSyntaxPrinter(raw_ostream &os) : os(os) {}

  /// Print a given identifier. If the identifer conflicts with a keyword, add a
  /// trailing underscore.
  void printIdentifier(StringRef name);

  /// Print the base name of the given declaration.
  void printBaseName(const ValueDecl *decl);

  /// Print the C-style prefix for the given module name, that's used for
  /// C type names inside the module.
  void printModuleNameCPrefix(const ModuleDecl &mod);

  /// Print the optional namespace qualifiers for the given module reference if
  /// it's not the same as the current context.
  void
  printModuleNamespaceQualifiersIfNeeded(const ModuleDecl *referencedModule,
                                         const ModuleDecl *currentContext);

  /// Print out additional C++ `template` and `requires` clauses that
  /// are required to emit a member definition outside  a C++ class that is
  /// generated for the given Swift type declaration.
  ///
  /// \returns true if nothing was printed.
  ///
  /// Examples:
  ///    1) For Swift's `String` type, it will print nothing.
  ///    2) For Swift's `Array<T>` type, it will print `template<class
  ///    T_0_0>\nrequires swift::isUsableInGenericContext<T_0_0>\n`
  bool printNominalTypeOutsideMemberDeclTemplateSpecifiers(
      const NominalTypeDecl *typeDecl);

  /// Print out the C++ class access qualifier for the given Swift  type
  /// declaration.
  ///
  /// Examples:
  ///    1) For Swift's `String` type, it will print `String
  ///    2) For Swift's `Array<T>` type, it will print `Array<T_0_0>
  ///    3) For Swift's `Array<T>.Index` type, it will print
  ///    `Array<T_0_0>::Index` 4) For Swift's `String` type in another module,
  ///    it will print `Swift::String`
  void printNominalTypeReference(const NominalTypeDecl *typeDecl,
                                 const ModuleDecl *moduleContext);

  /// Print out the C++ class access qualifier for the given Swift  type
  /// declaration.
  ///
  /// Examples:
  ///    1) For Swift's `String` type, it will print `String::`.
  ///    2) For Swift's `Array<T>` type, it will print `Array<T_0_0>::`
  ///    3) For Swift's `Array<T>.Index` type, it will print
  ///    `Array<T_0_0>::Index::` 4) For Swift's `String` type in another module,
  ///    it will print `Swift::String::`
  void printNominalTypeQualifier(const NominalTypeDecl *typeDecl,
                                 const ModuleDecl *moduleContext);

  /// Print a C++ namespace declaration with the give name and body.
  void
  printNamespace(llvm::function_ref<void(raw_ostream &OS)> namePrinter,
                 llvm::function_ref<void(raw_ostream &OS)> bodyPrinter) const;

  void
  printNamespace(StringRef name,
                 llvm::function_ref<void(raw_ostream &OS)> bodyPrinter) const;

  /// Print an extern C block with given body.
  void
  printExternC(llvm::function_ref<void(raw_ostream &OS)> bodyPrinter) const;

  /// Print an #ifdef __OBJC__ block.
  void
  printObjCBlock(llvm::function_ref<void(raw_ostream &OS)> bodyPrinter) const;

  /// Print the `swift::_impl::` namespace qualifier.
  void printSwiftImplQualifier() const;

  /// Where nullability information should be printed.
  enum class NullabilityPrintKind {
    Before,
    After,
    ContextSensitive,
  };

  void printInlineForThunk() const;

  void printNullability(
      Optional<OptionalTypeKind> kind,
      NullabilityPrintKind printKind = NullabilityPrintKind::After) const;

  /// Returns true if \p name matches a keyword in any Clang language mode.
  static bool isClangKeyword(StringRef name);
  static bool isClangKeyword(Identifier name);

  /// Print the call expression to the Swift type metadata access function.
  void printSwiftTypeMetadataAccessFunctionCall(
      StringRef name, ArrayRef<GenericRequirement> requirements);

  /// Print the set of statements to access the value witness table pointer
  /// ('vwTable') from the given type metadata variable.
  void printValueWitnessTableAccessSequenceFromTypeMetadata(
      StringRef metadataVariable, StringRef vwTableVariable, int indent);

  /// Print the metadata accessor function for the given type declaration.
  void printCTypeMetadataTypeFunction(
      const NominalTypeDecl *typeDecl, StringRef typeMetadataFuncName,
      llvm::ArrayRef<GenericRequirement> genericRequirements);

  /// Print the name of the generic type param type in C++.
  void printGenericTypeParamTypeName(const GenericTypeParamType *gtpt);

  /// Print the Swift generic signature as C++ template declaration alongside
  /// its requirements.
  void printGenericSignature(const CanGenericSignature &signature);

  /// Print the C++ template parameters that should be passed for a given
  /// generic signature.
  void printGenericSignatureParams(const CanGenericSignature &signature);

  /// Print the call to the C++ type traits that computes the underlying type /
  /// witness table pointer value that are passed to Swift for the given generic
  /// requirement.
  void
  printGenericRequirementInstantiantion(const GenericRequirement &requirement);

  /// Print the list of calls to C++ type traits that compute the generic
  /// pointer values to pass to Swift.
  void printGenericRequirementsInstantiantions(
      ArrayRef<GenericRequirement> requirements,
      LeadingTrivia leadingTrivia = LeadingTrivia::None);

  // Print the C++ type name that corresponds to the primary user facing C++
  // class for the given nominal type.
  void printPrimaryCxxTypeName(const NominalTypeDecl *type,
                               const ModuleDecl *moduleContext);

protected:
  raw_ostream &os;
};

} // end namespace swift

#endif
