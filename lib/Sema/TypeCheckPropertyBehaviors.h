//===--- CSDiagnostics.h - Constraint Diagnostics -------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file provides utilities for working with property behaviors.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SEMA_TYPECHECKPROPERTYBEHAVIORS_H
#define SWIFT_SEMA_TYPECHECKPROPERTYBEHAVIORS_H

#include <swift/AST/Type.h>
#include "TypeCheckType.h"

namespace swift {

class VarDecl;

/// Given the type of the given variable (which must have a property
/// behavior), form the stored property type that results from applying
/// the behavior.
Type applyPropertyBehaviorType(Type type, VarDecl *var,
                               TypeResolution resolution);

/// Retrieve the property declaration within a property behavior that
/// will be accessed to unwrap references to the variable.
VarDecl *getPropertyBehaviorUnwrapProperty(VarDecl *var);

} // end namespace

#endif /* SWIFT_SEMA_TYPECHECKPROPERTYBEHAVIORS_H */
