//===--- FoundationShims.h - Foundation declarations for core stdlib ------===//
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
//  In order to prevent a circular module dependency between the core
//  standard library and the Foundation overlay, we import these
//  declarations as part of SwiftShims.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STDLIB_SHIMS_FOUNDATIONSHIMS_H
#define SWIFT_STDLIB_SHIMS_FOUNDATIONSHIMS_H

//===--- Layout-compatible clones of Foundation structs -------------------===//
// Ideally we would declare the same names as Foundation does, but
// swift's module importer is not yet tolerant of the same struct
// coming in from two different Clang modules
// (rdar://problem/16294674).  Instead, we copy the definitions here
// and then do horrible unsafeBitCast trix to make them usable where required.
//===----------------------------------------------------------------------===//

#include "SwiftStdint.h"

typedef struct {
  __swift_intptr_t location;
  __swift_intptr_t length;
} _SwiftNSRange;

#ifdef __OBJC2__
typedef struct {
    unsigned long state;
    id __unsafe_unretained *itemsPtr;
    unsigned long *mutationsPtr;
    unsigned long extra[5];
} _SwiftNSFastEnumerationState;
#endif

// This struct is layout-compatible with NSOperatingSystemVersion.
typedef struct {
  __swift_intptr_t majorVersion;
  __swift_intptr_t minorVersion;
  __swift_intptr_t patchVersion;
} _SwiftNSOperatingSystemVersion;

_SwiftNSOperatingSystemVersion _swift_stdlib_operatingSystemVersion();

#endif // SWIFT_STDLIB_SHIMS_FOUNDATIONSHIMS_H

