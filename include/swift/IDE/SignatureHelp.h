//===--- SignatureHelp.h --- ------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IDE_SIGNATURE_HELP_H
#define SWIFT_IDE_SIGNATURE_HELP_H

#include "swift/AST/Type.h"
#include "swift/Basic/LLVM.h"
#include "swift/IDE/TypeCheckCompletionCallback.h"

namespace swift {
class IDEInspectionCallbacksFactory;

namespace ide {

struct SignatureHelpResult {
  struct Signature {
    /// True if this is a subscript rather than a function call.
    bool IsSubscript;
    
    /// The FuncDecl or SubscriptDecl associated with the call.
    ValueDecl *FuncD;
    
    /// The type of the function being called.
    AnyFunctionType *FuncTy;
    
    /// The index of the argument containing the completion location
    unsigned ArgIdx;
    
    /// The index of the parameter corresponding to the completion argument.
    std::optional<unsigned> ParamIdx;
    
    /// True if the completion is a noninitial term in a variadic argument.
    bool IsNoninitialVariadic;
    
    /// The base type of the call/subscript (null for free functions).
    Type BaseType;
    
    /// The resolved type of the expression.
    Type ExprType;
  };
  
  /// The decl context of the parsed expression.
  DeclContext *DC;

  /// Suggested signatures.
  SmallVector<Signature, 0> Signatures;
  
  SignatureHelpResult(DeclContext *DC) : DC(DC) {}
};

/// An abstract base class for consumers of signatures results.
class SignatureHelpConsumer {
public:
  virtual ~SignatureHelpConsumer() {}
  virtual void handleResult(const SignatureHelpResult &result) = 0;
};

/// Create a factory for code completion callbacks.
IDEInspectionCallbacksFactory *makeSignatureHelpCallbacksFactory(
    SignatureHelpConsumer &Consumer);

} // namespace ide
} // namespace swift

#endif // SWIFT_IDE_SIGNATURE_HELP_H
