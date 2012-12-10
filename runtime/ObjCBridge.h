//===--- ObjCBridge.h - Swift Language Objective-C Bridging ABI -*- C++ -*-===//
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
// Swift ABI for interacting with Objective-C.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_ABI_OBJCBRIDGE_H
#define SWIFT_ABI_OBJCBRIDGE_H

struct objc_class;

namespace swift {

struct Metadata;

/// \brief Fetch the type metadata associated with the formal dynamic
/// type of the given (possibly Objective-C) object.  The formal
/// dynamic type ignores dynamic subclasses such as those introduced
/// by KVO.
///
/// The object pointer may be a tagged pointer, but cannot be null.
extern "C" const Metadata *swift_getObjectType(id object);

} // end namespace swift

#endif /* SWIFT_ABI_OBJCBRIDGE_H */
