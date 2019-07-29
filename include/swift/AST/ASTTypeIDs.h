//===--- ASTTypeIDs.h - AST Type Ids ----------------------------*- C++ -*-===//
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
//  This file defines TypeID support for AST types.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_ASTTYPEIDS_H
#define SWIFT_AST_ASTTYPEIDS_H

#include "swift/Basic/TypeID.h"
namespace swift {

class CustomAttr;
class NominalTypeDecl;
struct PropertyWrapperBackingPropertyInfo;
struct PropertyWrapperTypeInfo;
enum class CtorInitializerKind;
struct PropertyWrapperMutability;
class Type;
class VarDecl;
class TypeAliasDecl;
class Type;

#define SWIFT_AST_TYPEID_ZONE 1

// Define the AST type zone (zone 1)
#define SWIFT_TYPEID_ZONE SWIFT_AST_TYPEID_ZONE
#define SWIFT_TYPEID_HEADER "swift/AST/ASTTypeIDZone.def"
#include "swift/Basic/DefineTypeIDZone.h"

} // end namespace swift

#endif // SWIFT_AST_ASTTYPEIDS_H
