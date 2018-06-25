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
  Type getTensorHandleElementType(Type ty);

  /// This enum is for type queries to check to see whether a given type is one
  /// of our known types that get moved to the graph.
  enum class TFValueKind {
    Nope,           ///< This is not a TensorFlow value.
    TensorHandle,   ///< This is TensorHandle<T>
    ResourceHandle, ///< This is ResourceHandle
    VariantHandle,  ///< This is VariantHandle
  };

  /// Determine whether the specified type is one of our well-known types, and
  /// if so, which one it is.
  TFValueKind classifyTensorFlowValue(Type ty);

  /// Return true if the specified type is a TensorHandle<T>.
  bool isTensorHandle(Type ty);

  /// Return true if the specified type is TensorHandle<T>, ResourceHandle, or
  /// VariantHandle.
  bool isTensorFlowValue(Type ty);

  /// Returns true if the specified type is a TensorFlow value or an tuple or
  /// struct of such.
  bool isTensorFlowValueOrAggregate(Type ty);

  /// This class provides an efficient implementation of a predicate that
  /// determines whether a type is or contains a TensorFlow value that will be
  /// exposed after deabstraction.  This is a class instead of a simple function
  /// because we memoize state to avoid rechecking types over and over again.
  class TypeContainsTensorFlowValue {
    /// This map memoizes whether the specified type declaration is known to
    /// contain an interesting type or not, used to accelerate queries against
    /// types that are frequently referenced (like Tensor).
    llvm::DenseMap<NominalTypeDecl*, bool> declContainsTensorFlowValue;
  public:
    TypeContainsTensorFlowValue() {}

    /// Return true if the specified type contains a TensorFlow value type that
    /// will be exposed after deabstraction.
    bool containsTensorFlowValue(Type ty);

  private:
    bool structContainsTensorFlowValue(StructDecl *decl);
  };

} // end namespace tf
} // end namespace swift
#endif
