//===--- LazyResolver.h - Lazy Resolution for ASTs --------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the LazyResolver abstract interface.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_LAZYRESOLVER_H
#define SWIFT_AST_LAZYRESOLVER_H

namespace swift {

class Identifier;
class ProtocolConformance;
class ProtocolDecl;
class Type;

/// Abstract interface used to lazily resolve aspects of the AST, such as the
/// types of declarations or protocol conformance structures.
class LazyResolver {
public:
  virtual ~LazyResolver();

  /// Resolve the conformance of the given type to the given protocol.
  ///
  /// \param type A type that should conform to the given protocol.
  /// \param protocol The protocol to which the type conforms.
  ///
  /// \returns the protocol conformance, or nullptr if no such conformance can
  /// be computed.
  ///
  /// FIXME: Eventually, this routine should only handle
  virtual ProtocolConformance *resolveConformance(Type type,
                                                  ProtocolDecl *protocol) = 0;

  /// Resolve a member type.
  ///
  /// \param type The type in which we will search for the member type.
  /// \param name The name of the member type.
  ///
  /// \returns the member type, or an empty type if no such type could be
  /// found.
  virtual Type resolveMemberType(Type type, Identifier name) = 0;
};

}

#endif // LLVM_SWIFT_AST_LAZYRESOLVER_H
