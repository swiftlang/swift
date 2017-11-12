//===--- MangleHack.h - Swift Mangler hack for various clients --*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Swift support for Interface Builder
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_MANGLEHACK_H
#define SWIFT_MANGLEHACK_H

// This returns a C string that must be deallocated with free().
extern "C" const char *
_swift_mangleSimpleClass(const char *module, const char *class_);

// This returns a C string that must be deallocated with free().
extern "C" const char *
_swift_mangleSimpleProtocol(const char *module, const char *protocol);

#endif
