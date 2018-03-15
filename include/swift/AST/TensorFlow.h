//===--- TensorFlow.h - AST Level TensorFlow Support Logic ------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the AST level TensorFlow support logic that is used across
// the Swift compiler.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_TENSORFLOW_H
#define SWIFT_AST_TENSORFLOW_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {
  class NominalTypeDecl;
  class StructDecl;
  class Type;

namespace tf {
  /// If the specified type is the well-known TensorHandle<T> type, then return
  /// "T".  If not, return a null type.
  Type isTensorHandle(Type ty);

  /// This class provides an efficient implementation of a predicate that
  /// determines whether a type is or contains a TensorHandle that will be
  /// exposed after deabstraction.  This is a class instead of a simple function
  /// because we memoize state to avoid rechecking types over and over again.
  class TypeContainsTensorHandle {
    /// This map memoizes whether the specified type declaration is known to
    /// contain a TensorHandle or not, used to accelerate queries against types
    /// that are frequently referenced like Tensor.
    llvm::DenseMap<NominalTypeDecl*, bool> declContainsTensorHandle;
  public:
    TypeContainsTensorHandle() {}

    /// Return true if the specified type contains a TensorHandle that will be
    /// exposed after deabstraction.
    bool containsTensorHandle(Type ty);

  private:
    bool structContainsTensorHandle(StructDecl *decl);
  };

} // end namespace tf
} // end namespace swift
#endif
