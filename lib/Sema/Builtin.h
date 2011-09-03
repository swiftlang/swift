//===--- Builtin.h - Builtin Declarations -----------------------*- C++ -*-===//
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
//  This file defines an interface for accessing various builtin types
//  and functions.
//
//===----------------------------------------------------------------------===//

#ifndef SEMA_BUILTIN_H
#define SEMA_BUILTIN_H

namespace swift {

class ASTContext;
class Identifier;
class Type;
class ValueDecl;

Type getBuiltinType(ASTContext &Context, Identifier Name);
ValueDecl *getBuiltinValue(ASTContext &Context, Identifier Name);
  
} // end namespace swift

#endif
