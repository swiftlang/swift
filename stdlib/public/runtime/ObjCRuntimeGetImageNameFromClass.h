//===--- ObjCRuntimeGetImageNameFromClass.h - ObjC hook setup ---*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_OBJCRUNTIMEGETIMAGENAMEFROMCLASS_H
#define SWIFT_RUNTIME_OBJCRUNTIMEGETIMAGENAMEFROMCLASS_H

#include "swift/Runtime/Config.h"

namespace swift {

#if SWIFT_OBJC_INTEROP
/// Set up class_getImageName so that it will understand Swift classes.
///
/// This function should only be called once per process.
void setUpObjCRuntimeGetImageNameFromClass();
#endif // SWIFT_OBJC_INTEROP

} // end namespace swift

#endif // SWIFT_RUNTIME_OBJCRUNTIMEGETIMAGENAMEFROMCLASS_H
