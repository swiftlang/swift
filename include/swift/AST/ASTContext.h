//===--- ASTContext.h - AST Context Object ----------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the ASTContext interface.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_ASTCONTEXT_H
#define SWIFT_AST_ASTCONTEXT_H

#include "llvm/Support/DataTypes.h"
#include "swift/AST/Type.h"
#include <vector>

namespace llvm {
  class SourceMgr;
}

namespace swift {
  class SourceLoc;
  class Type;
  class OneOfType;
  class TupleType;
  class FunctionType;
  class ArrayType;
  class Identifier;
  class Module;
  class TupleTypeElt;
  class OneOfElementDecl;
  class DiagnosticEngine;
  
/// ASTContext - This object creates and owns the AST objects.
class ASTContext {
  ASTContext(const ASTContext&) = delete;
  void operator=(const ASTContext&) = delete;

public:
  // Members that should only be used by ASTContext.cpp.
  struct Implementation;
  Implementation &Impl;
public:
  
  ASTContext(llvm::SourceMgr &SourceMgr, DiagnosticEngine &Diags);
  ~ASTContext();
  
  /// SourceMgr - The source manager object.
  llvm::SourceMgr &SourceMgr;

  /// \Diags - The diagnostics engine.
  DiagnosticEngine &Diags;
  
  /// TheBuiltinModule - The builtin module.
  Module * const TheBuiltinModule;

  /// ImportSearchPaths - The paths to search for imports in.
  std::vector<std::string> ImportSearchPaths;

  /// Allocate - Allocate memory from the ASTContext bump pointer.
  void *Allocate(unsigned long Bytes, unsigned Alignment);

  template <typename T>
  T *Allocate(unsigned NElts) {
    T *Res = (T*)Allocate(sizeof(T)*NElts, __alignof__(T));
    for (unsigned i = 0; i != NElts; ++i)
      new (Res+i) T();
    return Res;
  }

  template <typename T, typename It>
  T *AllocateCopy(It Start, It End) {
    T *Res = (T*)Allocate(sizeof(T)*(End-Start), __alignof__(T));
    for (unsigned i = 0; Start != End; ++Start, ++i)
      new (Res+i) T(*Start);
    return Res;
  }

  template<typename T>
  ArrayRef<T> AllocateCopy(ArrayRef<T> Arr) {
    return ArrayRef<T>(AllocateCopy<T>(Arr.begin(), Arr.end()),
                             Arr.size());
  }
  
  template<typename T>
  MutableArrayRef<T> AllocateCopy(MutableArrayRef<T> Arr) {
    return MutableArrayRef<T>(AllocateCopy<T>(Arr.begin(), Arr.end()),
                       Arr.size());
  }


  template<typename T>
  ArrayRef<T> AllocateCopy(const SmallVectorImpl<T> &Vec) {
    return AllocateCopy(ArrayRef<T>(Vec));
  }

  template<typename T>
  MutableArrayRef<T> AllocateCopy(SmallVectorImpl<T> &Vec) {
    return AllocateCopy(MutableArrayRef<T>(Vec));
  }

  
  /// getIdentifier - Return the uniqued and AST-Context-owned version of the
  /// specified string.
  Identifier getIdentifier(StringRef Str);

  //===--------------------------------------------------------------------===//
  // Diagnostics Helper functions
  //===--------------------------------------------------------------------===//

  bool hadError() const;
  
  //===--------------------------------------------------------------------===//
  // Type manipulation routines.
  //===--------------------------------------------------------------------===//

  // Builtin type and simple types that are used frequently.
  const Type TheErrorType;       /// TheErrorType - This is the error singleton.
  const Type TheEmptyTupleType;  /// TheEmptyTupleType - This is "()"
  
  /// TheDependentType - Dependent on context.  This is given to an anonymous
  /// closure argument (e.g. $4) and to UnresolvedMemberExprs (e.g. :foo) during
  /// type checking until they are resolved to something with concrete type.
  const Type TheDependentType;
  const Type TheIEEE32Type;     /// TheIEEE32Type  - 32-bit IEEE floating point
  const Type TheIEEE64Type;     /// TheIEEE64Type  - 64-bit IEEE floating point
  
  // Target specific types.
  const Type TheIEEE16Type;     /// TheIEEE16Type  - 16-bit IEEE floating point
  const Type TheIEEE80Type;     /// TheIEEE80Type  - 80-bit IEEE floating point
  const Type TheIEEE128Type;    /// TheIEEE128Type - 128-bit IEEE floating point
  const Type ThePPC128Type;     /// ThePPC128Type  - 128-bit PowerPC 2xDouble
};
  
} // end namespace swift

#endif
