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

#include <cstdint>

namespace swift {

class Decl;
class SourceFile;

/// Performs access-related checks for \p D.
///
/// At a high level, this checks the given declaration's signature does not
/// reference any other declarations that are less visible than the declaration
/// itself. Related checks may also be performed.
void checkAccessControl(Decl *D);

// Problematic origin of an exported type.
//
// This enum must be kept in sync with
// diag::decl_from_hidden_module and
// diag::conformance_from_implementation_only_module.
enum class DisallowedOriginKind : uint8_t {
  ImplementationOnly,
  SPIImported,
  SPILocal,
  None
};

/// Returns the kind of origin, implementation-only import or SPI declaration,
/// that restricts exporting \p decl from the given file and context.
DisallowedOriginKind getDisallowedOriginKind(const Decl *decl,
                                             const SourceFile &userSF,
                                             const Decl *userContext);

} // end namespace swift

#endif
