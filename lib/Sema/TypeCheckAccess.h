//===--- TypeCheckAccess.h - Type Checking for Access Control --*- C++ -*-===//
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
// This file implements access control checking.
//
//===----------------------------------------------------------------------===//

#ifndef TYPECHECKACCESS_H
#define TYPECHECKACCESS_H

namespace swift {

class Decl;
class TypeChecker;

/// Checks the given declaration's signature does not reference any other
/// declarations that are less visible than the declaration itself.
///
/// \p D must be a ValueDecl or a Decl that can appear in a type context.
void checkAccessControl(TypeChecker &TC, Decl *D);

/// Checks that the generic parameters of the given extension do not reference
/// any other declarations that are less visible than the user-specified access
/// on the extension.
void checkExtensionGenericParamAccess(TypeChecker &TC, const ExtensionDecl *ED,
                                      AccessLevel userSpecifiedAccess);

} // end namespace swift

#endif
