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
  class CanType;
  class NominalTypeDecl;
  class StructDecl;
  class Type;

namespace tf {
  /// Return true if the given type represents a TensorFlow dtype.
  bool isTensorFlowDType(Type ty);

  /// This function maps a Swift type (either a language type like Float or an
  /// LLVM Builtin type like Builtin.f32) into the TensorFlow TF_DataType value.
  unsigned convertSwiftTypeToTF(Type ty);

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
  
  /// Return true if the specified type is an opaque handle such as
  /// VariantHandle and ResourceHandle.
  bool isOpaqueHandle(Type ty);

  /// Return true if the specified type is TensorHandle<T>, ResourceHandle, or
  /// VariantHandle.
  bool isTensorFlowValue(Type ty);

  /// Return true if the specified type is a TensorFlow value or an tuple or
  /// struct of such.
  bool isTensorFlowValueOrAggregate(Type ty);

  /// Flatten an aggregate of TensorFlow value types and push them to the result
  /// array. If the given type itself is a TensorFlow value type, it will be
  /// pushed to the array. If any terminal type is not a TensorFlow value type,
  /// return false and the result will be empty.
  bool flattenTensorFlowValueAggregate(Type ty, SmallVectorImpl<Type> &result);

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
    /// If `checkHigherOrderFunctions`, also check for a function-typed `ty`, if
    /// its parameter or result contains any TensorFlow value type.
    bool containsTensorFlowValue(Type ty, bool checkHigherOrderFunctions);

  private:
    bool structContainsTensorFlowValue(StructDecl *decl);
  };

  /// This class provides a single source of truth for the set of types that are
  /// allowed as #tfop attributes, to ensure that all passes handle the same
  /// sets of #tfop attributes.
  class AttributeTypeClassifier {
  public:
    /// A classification of all the types that NormalAttributes support.
    enum class Normal {
      Bool,
      Int64,
      Double,
      Float,
      String,
      BoolArray,
      Int32Array,
      Int64Array,
      DoubleArray,
      FloatArray,
      StringArray,
      TensorShapeArray,
      OptionalTensorShapeArray,
      Function,

      /// Marker for types that NormalAttribute does not support.
      Unsupported
    };

    /// An English list of all the types that NormalAttribute supports, for use
    /// in diagnostics.
    static constexpr char normalSupportedTypesDesc[] =
        "Bool, Int64, Double, Float, String, array thereof, [TensorShape?], "
        "or Function";

    /// A classification of all the types that ShapeAttributes support.
    enum class Shape {
      TensorShape,
      OptionalTensorShape,

      /// Marker for types that ShapeAttribute does not support.
      Unsupported
    };

    /// An English list of all the types that ShapeAttribute supports, for use
    /// in diagnostics.
    static constexpr char shapeSupportedTypesDesc[] =
        "TensorShape or TensorShape?";

    /// A classification of all the types that TFDataTypeAttributes support.
    enum class TFDataType {
      TensorDataType,
      TensorDataTypeArray,

      /// Marker for types that TFDataTypeAttribute does not support.
      Unsupported
    };

    /// An English list of all the types that TFDataTypeAttribute supports, for
    /// use in diagnostics.
    static constexpr char tfDataTypeSupportedTypesDesc[] =
        "TensorDataType or [TensorDataType]";

    /// Classify `type` for use as a NormalAttribute.
    Normal classifyNormalAttribute(Type type);

    /// Classify `type` for use as a ShapeAttribute.
    Shape classifyShapeAttribute(Type type);

    /// Classify `type` for use as a TFDataTypeAttribute.
    TFDataType classifyTFDataTypeAttribute(Type type);

  private:
    llvm::DenseMap<CanType, Normal> normalAttributeTypes;
    llvm::DenseMap<CanType, Shape> shapeAttributeTypes;
    llvm::DenseMap<CanType, TFDataType> tfDataTypeAttributeTypes;
  };

} // end namespace tf
} // end namespace swift
#endif
