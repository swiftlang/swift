//===--- DerivedConformances.h - Derived protocol conformance ---*- C++ -*-===//
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
//  This file defines entry points to synthesize compiler-derived conformances
//  to certain known protocols.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_DERIVEDCONFORMANCES_H
#define SWIFT_SEMA_DERIVEDCONFORMANCES_H

namespace swift {
  class NominalTypeDecl;
  class TypeChecker;
  
namespace DerivedConformance {

// Synthesize a RawRepresentable conformance for an enum, if it has a valid
// raw type and raw values for all of its cases.
//
// \returns false if the derivation was successful, in which case the needed
//          implicit declarations will be added to the type AST. True if the
//          derivation failed, in which case the type AST is unchanged.
bool deriveRawRepresentable(TypeChecker &tc,
                            NominalTypeDecl *type);

}
}

#endif