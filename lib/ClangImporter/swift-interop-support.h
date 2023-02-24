//===--- swift-interop-support.h - C++ and Swift Interop --------*- C++ -*-===//
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
// This file provides common utilities and annotations that are useful for C++
// codebases that interoperate with Swift.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_CLANGIMPORTER_SWIFT_INTEROP_SUPPORT_H
#define SWIFT_CLANGIMPORTER_SWIFT_INTEROP_SUPPORT_H

#define SELF_CONTAINED __attribute__((swift_attr("import_owned")))
#define SAFE_TO_IMPORT __attribute__((swift_attr("import_unsafe")))

#define _CXX_INTEROP_STRINGIFY(_x) #_x
#define REFERENCE(_retain, _release)                                           \
  __attribute__((swift_attr("import_reference")))                              \
  __attribute__((swift_attr(_CXX_INTEROP_STRINGIFY(retain:_retain))))          \
  __attribute__((swift_attr(_CXX_INTEROP_STRINGIFY(release:_release))))

#define SWIFT_NAME(_name) __attribute__((swift_name(#_name)))

#endif // SWIFT_CLANGIMPORTER_SWIFT_INTEROP_SUPPORT_H
