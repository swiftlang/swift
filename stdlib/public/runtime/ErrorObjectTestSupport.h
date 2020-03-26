//===--- ErrorObjectTestSupport.h - Support for Instruments.app -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Swift runtime support for tests involving errors.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_ERROROBJECT_TEST_SUPPORT_H
#define SWIFT_RUNTIME_ERROROBJECT_TEST_SUPPORT_H

namespace swift {

SWIFT_RUNTIME_EXPORT void (*_swift_willThrow)(SwiftError *error);

}

#endif
