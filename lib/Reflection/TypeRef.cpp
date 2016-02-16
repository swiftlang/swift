//===--- TypeRef.h - Swift Type References for Reflection -------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Implements the structures of type references for property and enum
// case reflection.
//
//===----------------------------------------------------------------------===//

#include "swift/Reflection/TypeRef.h"

using namespace swift;
using namespace reflection;

const std::shared_ptr<ForeignClassTypeRef>
ForeignClassTypeRef::Unnamed = std::make_shared<ForeignClassTypeRef>("");

const std::shared_ptr<ObjCClassTypeRef>
ObjCClassTypeRef::Unnamed = std::make_shared<ObjCClassTypeRef>("");

const std::shared_ptr<OpaqueTypeRef>
OpaqueTypeRef::Opaque = std::make_shared<OpaqueTypeRef>();
