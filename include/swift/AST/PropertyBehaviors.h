//===--- PropertyBehaviors.h - Property Behavior ASTs -----------*- C++ -*-===//
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
// This file defines helper types for property behaviors.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_PROPERTY_BEHAVIORS_H
#define SWIFT_AST_PROPERTY_BEHAVIORS_H

namespace llvm {
  class raw_ostream;
}

namespace swift {

class VarDecl;
class ConstructorDecl;
  
/// Describes a property behavior type.
struct PropertyBehaviorTypeInfo {
  /// The property through which we should "unwrap" the behavior type to access
  /// the underlying value.
  VarDecl *unwrapProperty = nullptr;

  /// The initializer init(initialValue:) that will be called when the
  /// initiqlizing the property behavior type from a value of the property type.
  ConstructorDecl *initialValueInit = nullptr;

  /// Whether this is a valid property behavior.
  bool isValid() const {
    return unwrapProperty != nullptr;
  }

  explicit operator bool() const { return isValid(); }

  friend bool operator==(const PropertyBehaviorTypeInfo &lhs,
                         const PropertyBehaviorTypeInfo &rhs) {
    return lhs.unwrapProperty == rhs.unwrapProperty &&
        lhs.initialValueInit == rhs.initialValueInit;
  }
};

void simple_display(
    llvm::raw_ostream &out, const PropertyBehaviorTypeInfo &propertyBehavior);

} // end namespace swift

#endif // SWIFT_AST_PROPERTY_BEHAVIORS_H
