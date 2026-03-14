//===--- FormalLinkage.h - Formal linkage of types and decls ----*- C++ -*-===//
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

#ifndef SWIFT_SIL_FORMALLINKAGE_H
#define SWIFT_SIL_FORMALLINKAGE_H

namespace swift {

class CanGenericSignature;
class CanType;
class ProtocolConformance;
class ValueDecl;
enum class SILLinkage : unsigned char;
enum ForDefinition_t : bool;

/// Formal linkage is a property of types and declarations that
/// informs, but is not completely equivalent to, the linkage of
/// symbols corresponding to those types and declarations.
enum class FormalLinkage {
  /// This entity is visible in multiple Swift modules and has a
  /// unique file that is known to define it.
  PublicUnique,

  /// This entity is visible in multiple Swift modules, but does not
  /// have a unique file that is known to define it.
  PublicNonUnique,

  /// This entity is visible in multiple Swift modules within a package
  /// and has a unique file that is known to define it.
  PackageUnique,

  /// This entity is visible in only a single Swift module and has a
  /// unique file that is known to define it.
  HiddenUnique,

  /// This entity is visible in only a single Swift file. These are by
  /// definition unique.
  Private,
};

FormalLinkage getDeclLinkage(const ValueDecl *decl);
FormalLinkage getTypeLinkage(CanType formalType);
FormalLinkage getGenericSignatureLinkage(CanGenericSignature signature);
SILLinkage getSILLinkage(FormalLinkage linkage,
                         ForDefinition_t forDefinition);
SILLinkage
getLinkageForProtocolConformance(const ProtocolConformance *C,
                                 ForDefinition_t definition);

} // end swift namespace

#endif
