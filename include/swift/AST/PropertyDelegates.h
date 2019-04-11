//===--- PropertyDelegates.h - Property Delegate ASTs -----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines helper types for property delegates.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_PROPERTY_DELEGATES_H
#define SWIFT_AST_PROPERTY_DELEGATES_H

namespace llvm {
  class raw_ostream;
}

namespace swift {

class ConstructorDecl;
class CustomAttr;
class Expr;
class VarDecl;
class OpaqueValueExpr;

/// Describes a property delegate type.
struct PropertyDelegateTypeInfo {
  /// The property through to which access that uses this delegate type is
  /// delegated.
  VarDecl *valueVar = nullptr;

  /// The initializer init(initialValue:) that will be called when the
  /// initiqlizing the property delegate type from a value of the property type.
  ConstructorDecl *initialValueInit = nullptr;

  /// Whether this is a valid property delegate.
  bool isValid() const {
    return valueVar != nullptr;
  }

  explicit operator bool() const { return isValid(); }

  friend bool operator==(const PropertyDelegateTypeInfo &lhs,
                         const PropertyDelegateTypeInfo &rhs) {
    return lhs.valueVar == rhs.valueVar &&
        lhs.initialValueInit == rhs.initialValueInit;
  }
};

/// Describes the backing property of a property that has an attached delegate.
struct PropertyDelegateBackingPropertyInfo {
  /// The backing property.
  VarDecl *backingVar = nullptr;

  /// When the original default value is specified in terms of an '='
  /// initializer on the initial property, e.g.,
  ///
  /// \code
  /// @Lazy var i = 17
  /// \end
  ///
  /// This is the specified initial value (\c 17), which is suitable for
  /// embedding in the expression \c initializeFromOriginal.
  Expr *originalInitialValue = nullptr;

  /// An expression that initializes the backing property from a value of
  /// the original property's type (e.g., via `init(initialValue:)`), or
  /// \c NULL if the backing property can only be initialized directly.
  Expr *initializeFromOriginal = nullptr;

  /// When \c initializeFromOriginal is non-NULL, the opaque value that
  /// is used as a stand-in for a value of the original property's type.
  OpaqueValueExpr *underlyingValue = nullptr;

  PropertyDelegateBackingPropertyInfo() { }
  
  PropertyDelegateBackingPropertyInfo(VarDecl *backingVar,
                                      Expr *originalInitialValue,
                                      Expr *initializeFromOriginal,
                                      OpaqueValueExpr *underlyingValue)
    : backingVar(backingVar), originalInitialValue(originalInitialValue),
      initializeFromOriginal(initializeFromOriginal),
      underlyingValue(underlyingValue) { }

  /// Whether this is a valid property delegate.
  bool isValid() const {
    return backingVar != nullptr;
  }

  explicit operator bool() const { return isValid(); }

  friend bool operator==(const PropertyDelegateBackingPropertyInfo &lhs,
                         const PropertyDelegateBackingPropertyInfo &rhs) {
    // FIXME: Can't currently compare expressions.
    return lhs.backingVar == rhs.backingVar;
  }
};

void simple_display(
    llvm::raw_ostream &out,
    const PropertyDelegateTypeInfo &propertyDelegate);

void simple_display(
    llvm::raw_ostream &out,
    const PropertyDelegateBackingPropertyInfo &backingInfo);

} // end namespace swift

#endif // SWIFT_AST_PROPERTY_DELEGATES_H
