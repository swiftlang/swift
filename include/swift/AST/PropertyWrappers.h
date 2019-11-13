//===--- PropertyWrappers.h - Property Wrapper ASTs -----------*- C++ -*-===//
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
// This file defines helper types for property wrappers.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_PROPERTY_WRAPPERS_H
#define SWIFT_AST_PROPERTY_WRAPPERS_H

namespace llvm {
  class raw_ostream;
}

namespace swift {

class ConstructorDecl;
class CustomAttr;
class Expr;
class VarDecl;
class OpaqueValueExpr;

/// Describes a property wrapper type.
struct PropertyWrapperTypeInfo {
  /// The property through which access that uses this wrapper type is
  /// directed.
  VarDecl *valueVar = nullptr;

  /// Whether there is an init(wrappedValue:) that will be called when the
  /// initializing the property wrapper type from a value of the property type.
  enum {
    NoWrappedValueInit = 0,
    HasWrappedValueInit,
    HasInitialValueInit
  } wrappedValueInit = NoWrappedValueInit;

  /// The initializer that will be called to default-initialize a
  /// value with an attached property wrapper.
  enum {
    NoDefaultValueInit = 0,
    HasDefaultValueInit
  } defaultInit = NoDefaultValueInit;

  /// The property through which the projection value ($foo) will be accessed.
  ///
  /// This property is optional. If present, a computed property for `$foo`
  /// will be created that redirects to this property.
  VarDecl *projectedValueVar = nullptr;

  /// The static subscript through which the access of instance properties
  /// of classes can be directed (instead of wrappedValue), providing the
  /// ability to reason about the enclosing "self".
  SubscriptDecl *enclosingInstanceWrappedSubscript = nullptr;

  /// The static subscript through which the access of instance properties
  /// of classes can be directed (instead of projectedValue), providing the
  /// ability to reason about the enclosing "self".
  SubscriptDecl *enclosingInstanceProjectedSubscript = nullptr;

  ///
  /// Whether this is a valid property wrapper.
  bool isValid() const {
    return valueVar != nullptr;
  }

  explicit operator bool() const { return isValid(); }

  friend bool operator==(const PropertyWrapperTypeInfo &lhs,
                         const PropertyWrapperTypeInfo &rhs) {
    return lhs.valueVar == rhs.valueVar &&
        lhs.wrappedValueInit == rhs.wrappedValueInit;
  }
};

/// Describes the mutability of the operations on a property wrapper or composition.
struct PropertyWrapperMutability {
  enum Value: uint8_t {
    Nonmutating = 0,
    Mutating = 1,
    DoesntExist = 2,
  };
  
  Value Getter, Setter;
  
  /// Get the mutability of a composed access chained after accessing a wrapper with `this`
  /// getter and setter mutability.
  Value composeWith(Value x) {
    switch (x) {
    case DoesntExist:
      return DoesntExist;
    
    // If an operation is nonmutating, then its input relies only on the
    // mutating-ness of the outer wrapper's get operation.
    case Nonmutating:
      return Getter;
        
    // If it's mutating, then it relies
    // on a) the outer wrapper having a setter to exist at all, and b) the
    // mutating-ness of either the getter or setter, since we need both to
    // perform a writeback cycle.
    case Mutating:
      if (Setter == DoesntExist) {
        return DoesntExist;
      }
      return std::max(Getter, Setter);
    }
    llvm_unreachable("Unhandled Value in switch");
  }
  
  bool operator==(PropertyWrapperMutability other) const {
    return Getter == other.Getter && Setter == other.Setter;
  }
};

void simple_display(llvm::raw_ostream &os, PropertyWrapperMutability m);

/// Describes the backing property of a property that has an attached wrapper.
struct PropertyWrapperBackingPropertyInfo {
  /// The backing property.
  VarDecl *backingVar = nullptr;

  /// The storage wrapper property, if any. When present, this takes the name
  /// '$foo' from `backingVar`.
  VarDecl *storageWrapperVar = nullptr;

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
  /// the original property's type (e.g., via `init(wrappedValue:)`), or
  /// \c NULL if the backing property can only be initialized directly.
  Expr *initializeFromOriginal = nullptr;

  /// When \c initializeFromOriginal is non-NULL, the opaque value that
  /// is used as a stand-in for a value of the original property's type.
  OpaqueValueExpr *underlyingValue = nullptr;

  PropertyWrapperBackingPropertyInfo() { }
  
  PropertyWrapperBackingPropertyInfo(VarDecl *backingVar,
                                      VarDecl *storageWrapperVar,
                                      Expr *originalInitialValue,
                                      Expr *initializeFromOriginal,
                                      OpaqueValueExpr *underlyingValue)
    : backingVar(backingVar), storageWrapperVar(storageWrapperVar),
      originalInitialValue(originalInitialValue),
      initializeFromOriginal(initializeFromOriginal),
      underlyingValue(underlyingValue) { }

  /// Whether this is a valid property wrapper.
  bool isValid() const {
    return backingVar != nullptr;
  }

  explicit operator bool() const { return isValid(); }

  friend bool operator==(const PropertyWrapperBackingPropertyInfo &lhs,
                         const PropertyWrapperBackingPropertyInfo &rhs) {
    // FIXME: Can't currently compare expressions.
    return lhs.backingVar == rhs.backingVar;
  }
};

void simple_display(
    llvm::raw_ostream &out,
    const PropertyWrapperTypeInfo &propertyWrapper);

void simple_display(
    llvm::raw_ostream &out,
    const PropertyWrapperBackingPropertyInfo &backingInfo);

/// Given the initializer for the given property with an attached property
/// wrapper, dig out the original initialization expression.
///
/// Cannot just dig out the getOriginalInit() value because this function checks
/// types, etc. Erroneous code won't return a result from here.
Expr *findOriginalPropertyWrapperInitialValue(VarDecl *var, Expr *init);

} // end namespace swift

#endif // SWIFT_AST_PROPERTY_WRAPPERS_H
