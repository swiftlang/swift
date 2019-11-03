//===--- IDETypeIDs.h - IDE Type Ids ----------------------------*- C++ -*-===//
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
//  This file defines TypeID support for IDE types.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IDE_IDETYPEIDS_H
#define SWIFT_IDE_IDETYPEIDS_H

#include "swift/Basic/TypeID.h"
namespace swift {

#define SWIFT_TYPEID_ZONE IDETypes
#define SWIFT_TYPEID_HEADER "swift/IDE/IDETypeIDZone.def"
#include "swift/Basic/DefineTypeIDZone.h"

} // end namespace swift

#endif // SWIFT_IDE_IDETYPEIDS_H
