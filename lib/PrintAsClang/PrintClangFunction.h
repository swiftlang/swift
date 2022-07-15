//===--- PrintClangFunction.h - Printer for C/C++ functions -----*- C++ -*-===//
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

#ifndef SWIFT_PRINTASCLANG_PRINTCLANGFUNCTION_H
#define SWIFT_PRINTASCLANG_PRINTCLANGFUNCTION_H

#include "swift/AST/Type.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {

class AbstractFunctionDecl;
class AccessorDecl;
class FuncDecl;
class ModuleDecl;
class NominalTypeDecl;
class ParamDecl;
class ParameterList;
class PrimitiveTypeMapping;
class SwiftToClangInteropContext;

/// Responsible for printing a Swift function decl or type in C or C++ mode, to
/// be included in a Swift module's generated clang header.
class DeclAndTypeClangFunctionPrinter {
public:
  DeclAndTypeClangFunctionPrinter(raw_ostream &os, raw_ostream &cPrologueOS,
                                  PrimitiveTypeMapping &typeMapping,
                                  SwiftToClangInteropContext &interopContext)
      : os(os), cPrologueOS(cPrologueOS), typeMapping(typeMapping),
        interopContext(interopContext) {}

  /// What kind of function signature should be emitted for the given Swift
  /// function.
  enum class FunctionSignatureKind {
    /// Emit a signature for the C function prototype.
    CFunctionProto,
    /// Emit a signature for the inline C++ function thunk.
    CxxInlineThunk
  };

  /// Information about any additional parameters.
  struct AdditionalParam {
    enum class Role { Self, Error };

    Role role;
    Type type;
    // Should self be passed indirectly?
    bool isIndirect = false;
  };

  /// Optional modifiers that can be applied to function signature.
  struct FunctionSignatureModifiers {
    /// Additional qualifier to add before the function's name.
    const NominalTypeDecl *qualifierContext;

    FunctionSignatureModifiers() : qualifierContext(nullptr) {}
  };

  /// Print the C function declaration or the C++ function thunk that
  /// corresponds to the given function declaration.
  void printFunctionSignature(const AbstractFunctionDecl *FD, StringRef name,
                              Type resultTy, FunctionSignatureKind kind,
                              ArrayRef<AdditionalParam> additionalParams = {},
                              FunctionSignatureModifiers modifiers = {});

  /// Print the use of the C++ function thunk parameter as it's passed to the C
  /// function declaration.
  void printCxxToCFunctionParameterUse(const ParamDecl *param, StringRef name);

  /// Print the body of the inline C++ function thunk that calls the underlying
  /// Swift function.
  void printCxxThunkBody(StringRef swiftSymbolName,
                         const ModuleDecl *moduleContext, Type resultTy,
                         const ParameterList *params,
                         ArrayRef<AdditionalParam> additionalParams = {},
                         bool hasThrows = false);

  /// Print the Swift method as C++ method declaration/definition, including
  /// constructors.
  void printCxxMethod(const NominalTypeDecl *typeDeclContext,
                      const AbstractFunctionDecl *FD, StringRef swiftSymbolName,
                      Type resultTy, bool isDefinition);

  /// Print the C++ getter/setter method signature.
  void printCxxPropertyAccessorMethod(const NominalTypeDecl *typeDeclContext,
                                      const AccessorDecl *accessor,
                                      StringRef swiftSymbolName, Type resultTy,
                                      bool isDefinition);

private:
  void printCxxToCFunctionParameterUse(
      Type type, StringRef name, const ModuleDecl *moduleContext, bool isInOut,
      bool isIndirect = false,
      llvm::Optional<AdditionalParam::Role> paramRole = None);

  bool hasKnownOptionalNullableCxxMapping(Type type);

  raw_ostream &os;
  raw_ostream &cPrologueOS;
  PrimitiveTypeMapping &typeMapping;
  SwiftToClangInteropContext &interopContext;
};

} // end namespace swift

#endif
