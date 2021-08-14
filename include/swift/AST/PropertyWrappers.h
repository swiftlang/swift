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

/// The kind of property initializer to look for
enum class PropertyWrapperInitKind {
  /// An initial-value initializer (i.e. `init(initialValue:)`), which is
  /// deprecated.
  InitialValue,
  /// An wrapped-value initializer (i.e. `init(wrappedValue:)`)
  WrappedValue,
  /// A projected-value initializer (i.e. `init(projectedValue:)`)
  ProjectedValue,
  /// An default-value initializer (i.e. `init()` or `init(defaultArgs...)`)
  Default
};

/// Information about an applied property wrapper, including the backing wrapper type
/// and the initialization kind.
struct AppliedPropertyWrapper {
  Type wrapperType;
  PropertyWrapperInitKind initKind;
};

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

  bool hasProjectedValueInit = false;

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

  /// Forces that the property wrapper must be declared on a static, or
  /// global–once supported–property.
  bool requireNoEnclosingInstance = false;

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

/// Describes whether the reference to a property wrapper instance used for
/// accessing a wrapped property should be an l-value or not.
struct PropertyWrapperLValueness {
  llvm::SmallVector<bool, 4> isLValueForGetAccess;
  llvm::SmallVector<bool, 4> isLValueForSetAccess;

  PropertyWrapperLValueness(unsigned numWrappers)
      : isLValueForGetAccess(numWrappers), isLValueForSetAccess(numWrappers) {}

  bool operator==(PropertyWrapperLValueness other) const {
    return (isLValueForGetAccess == other.isLValueForGetAccess &&
            isLValueForSetAccess == other.isLValueForSetAccess);
  }
};

void simple_display(llvm::raw_ostream &os, PropertyWrapperLValueness l);

/// Given the initializer for a property with an attached property wrapper,
/// dig out the wrapped value placeholder for the original initialization
/// expression.
///
/// \note The wrapped value placeholder is injected for properties that can
/// be initialized out-of-line using an expression of the wrapped property type.
PropertyWrapperValuePlaceholderExpr *findWrappedValuePlaceholder(Expr *init);

/// The synthesized auxiliary declarations for a wrapped property, including the
/// backing property wrapper, the projected value variable, and if the wrapped
/// declaration is a parameter, the local wrapped value variable.
struct PropertyWrapperAuxiliaryVariables {
  /// The backing property.
  VarDecl *backingVar = nullptr;

  /// The synthesized projection property, if any. When present, this takes the name
  /// of the original wrapped property prefixed with \c $
  VarDecl *projectionVar = nullptr;

  /// The synthesized local wrapped value property, which shadows the original wrapped
  /// declaration if it is a parameter.
  VarDecl *localWrappedValueVar = nullptr;

  PropertyWrapperAuxiliaryVariables() {}

  PropertyWrapperAuxiliaryVariables(VarDecl *backingVar, VarDecl *projectionVar,
                                    VarDecl *localWrappedValueVar = nullptr)
      : backingVar(backingVar), projectionVar(projectionVar),
        localWrappedValueVar(localWrappedValueVar) {}

  /// Whether this is a valid property wrapper.
  bool isValid() const {
    return backingVar != nullptr;
  }

  explicit operator bool() const { return isValid(); }
};

/// Describes how to initialize the backing storage of a property with
/// an attached wrapper.
class PropertyWrapperInitializerInfo {
  struct {
    /// An expression that initializes the backing property from a value of
    /// the original property's type via \c init(wrappedValue:) if supported
    /// by the wrapper type.
    Expr *expr = nullptr;

    /// When \c expr is not null, the opaque value that is used as
    /// a placeholder for a value of the original property's type.
    PropertyWrapperValuePlaceholderExpr *placeholder = nullptr;
  } wrappedValueInit;

  struct {
    /// An expression that initializes the backing property from a value of
    /// the synthesized projection type via \c init(projectedValue:) if
    /// supported by the wrapper type.
    Expr *expr = nullptr;

    /// When \c expr is not null, the opaque value that is used as
    /// a placeholder for a value of the projection type.
    PropertyWrapperValuePlaceholderExpr *placeholder = nullptr;
  } projectedValueInit;

public:
  PropertyWrapperInitializerInfo() { }

  PropertyWrapperInitializerInfo(Expr *wrappedValueInitExpr,
                                 Expr *projectedValueInitExpr) {
    wrappedValueInit.expr = wrappedValueInitExpr;
    if (wrappedValueInitExpr) {
      wrappedValueInit.placeholder = findWrappedValuePlaceholder(wrappedValueInitExpr);
    }

    projectedValueInit.expr = projectedValueInitExpr;
    if (projectedValueInitExpr) {
      projectedValueInit.placeholder = findWrappedValuePlaceholder(projectedValueInitExpr);
    }
  }

  bool hasInitFromWrappedValue() const {
    return wrappedValueInit.expr != nullptr;
  }

  Expr *getInitFromWrappedValue() const {
    return wrappedValueInit.expr;
  }

  PropertyWrapperValuePlaceholderExpr *getWrappedValuePlaceholder() {
    return wrappedValueInit.placeholder;
  }

  bool hasInitFromProjectedValue() const {
    return projectedValueInit.expr != nullptr;
  }

  Expr *getInitFromProjectedValue() const {
    return projectedValueInit.expr;
  }

  PropertyWrapperValuePlaceholderExpr *getProjectedValuePlaceholder() {
    return projectedValueInit.placeholder;
  }

  bool hasSynthesizedInitializers() const {
    return hasInitFromWrappedValue() || hasInitFromProjectedValue();
  }
};

void simple_display(
    llvm::raw_ostream &out,
    const PropertyWrapperTypeInfo &propertyWrapper);

void simple_display(
    llvm::raw_ostream &out,
    const PropertyWrapperInitializerInfo &initInfo);

void simple_display(
    llvm::raw_ostream &out,
    const PropertyWrapperAuxiliaryVariables &auxiliaryVars);

} // end namespace swift

#endif // SWIFT_AST_PROPERTY_WRAPPERS_H
