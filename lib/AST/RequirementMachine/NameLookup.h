//===--- NameLookup.h - Name lookup utilities -------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RQM_NAMELOOKUP_H
#define SWIFT_RQM_NAMELOOKUP_H

#include "llvm/ADT/SmallVector.h"

namespace swift {

class Identifier;
class NominalTypeDecl;
class Type;
class TypeDecl;

namespace rewriting {

void lookupConcreteNestedType(
    Type baseType,
    Identifier name,
    llvm::SmallVectorImpl<TypeDecl *> &concreteDecls);

void lookupConcreteNestedType(
    NominalTypeDecl *decl,
    Identifier name,
    llvm::SmallVectorImpl<TypeDecl *> &concreteDecls);

TypeDecl *findBestConcreteNestedType(
    llvm::SmallVectorImpl<TypeDecl *> &concreteDecls);

} // end namespace rewriting

} // end namespace swift

#endif