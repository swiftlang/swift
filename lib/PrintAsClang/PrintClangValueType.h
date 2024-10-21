//===--- PrintClangValueType.h - Printer for C/C++ value types --*- C++ -*-===//
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

#ifndef SWIFT_PRINTASCLANG_PRINTCLANGVALUETYPE_H
#define SWIFT_PRINTASCLANG_PRINTCLANGVALUETYPE_H

#include "OutputLanguageMode.h"
#include "SwiftToClangInteropContext.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Type.h"
#include "swift/Basic/LLVM.h"
#include "swift/IRGen/GenericRequirement.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {

class DeclAndTypePrinter;
class ModuleDecl;
class NominalTypeDecl;
class PrimitiveTypeMapping;
class StructDecl;
class SwiftToClangInteropContext;

/// Responsible for printing a Swift struct or enum decl or in C or C++ mode, to
/// be included in a Swift module's generated clang header.
class ClangValueTypePrinter {
public:
  ClangValueTypePrinter(raw_ostream &os, raw_ostream &cPrologueOS,
                        SwiftToClangInteropContext &interopContext)
      : os(os), cPrologueOS(cPrologueOS), interopContext(interopContext), Context(interopContext.getASTContext()) {}

  /// Print the C++ class definition that
  /// corresponds to the given structure or enum declaration.
  void printValueTypeDecl(const NominalTypeDecl *typeDecl,
                          llvm::function_ref<void(void)> bodyPrinter,
                          DeclAndTypePrinter &declAndTypePrinter);

  /// Print the use of a C++ struct/enum parameter value as it's passed to the
  /// underlying C function that represents the native Swift function.
  void printParameterCxxToCUseScaffold(
      const ModuleDecl *moduleContext, llvm::function_ref<void()> typePrinter,
      llvm::function_ref<void()> cxxParamPrinter, bool isSelf);

  enum class TypeUseKind {
    // The name of the C++ class that corresponds to the Swift value type (with
    // any qualifiers).
    CxxTypeName,
    // The name of the C++ _impl class that corresponds to the Swift value type
    // (with any qualifiers).
    CxxImplTypeName
  };

  /// Print the return type that refers to a Swift struct type in C/C++.
  void printValueTypeReturnType(const NominalTypeDecl *typeDecl,
                                OutputLanguageMode outputLang,
                                TypeUseKind typeUse,
                                const ModuleDecl *moduleContext);

  /// Print the supporting code  that's required to indirectly return a C++
  /// class that represents a Swift value type as it's being indirectly passed
  /// from the C function that represents the native Swift function.
  void
  printValueTypeReturnScaffold(const NominalTypeDecl *typeDecl,
                               const ModuleDecl *moduleContext,
                               llvm::function_ref<void()> typePrinter,
                               llvm::function_ref<void(StringRef)> bodyPrinter);

  /// Print out the C++ type name of the implementation class that provides
  /// hidden access to the private class APIs.
  static void printCxxImplClassName(raw_ostream &os,
                                    const NominalTypeDecl *type);

  /// Print a variable that can be used to access type's metadata function
  static void printMetadataAccessAsVariable(const ASTContext &Context,
      raw_ostream &os, StringRef metadataFuncName,
      ArrayRef<GenericRequirement> genericRequirements, int indent = 4,
      StringRef varName = "metadata");

  /// Print a variable that can be used to access type's metadata function and
  /// value witness table
  static void printValueWitnessTableAccessAsVariable(const ASTContext &Context,
      raw_ostream &os, StringRef metadataFuncName,
      ArrayRef<GenericRequirement> genericRequirements, int indent = 4,
      StringRef metadataVarName = "metadata",
      StringRef vwTableVarName = "vwTable");

  static void printTypeGenericTraits(
      raw_ostream &os, const TypeDecl *typeDecl, StringRef typeMetadataFuncName,
      ArrayRef<GenericRequirement> typeMetadataFuncRequirements,
      const ModuleDecl *moduleContext, DeclAndTypePrinter &declAndTypePrinter,
      bool isOpaqueLayout = false);

  static void printTypePrecedingGenericTraits(raw_ostream &os,
                                              const NominalTypeDecl *typeDecl,
                                              const ModuleDecl *moduleContext);

  static void forwardDeclType(raw_ostream &os, const NominalTypeDecl *typeDecl,
                              DeclAndTypePrinter &declAndTypePrinter);

  /// Print out the type traits that allow a C++ type be used a Swift generic
  /// context.
  static void
  printClangTypeSwiftGenericTraits(raw_ostream &os, const TypeDecl *typeDecl,
                                   const ModuleDecl *moduleContext,
                                   DeclAndTypePrinter &declAndTypePrinter);

private:
  raw_ostream &os;
  raw_ostream &cPrologueOS;
  SwiftToClangInteropContext &interopContext;
  const ASTContext &Context;
};

} // end namespace swift

#endif
