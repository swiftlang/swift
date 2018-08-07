//===--- ReferenceDependencyKeys.h - Keys for swiftdeps files ---*- C++ -*-===//
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

#ifndef SWIFT_FRONTENDTOOL_REFERENCEDEPENDENCYKEYS_H
#define SWIFT_FRONTENDTOOL_REFERENCEDEPENDENCYKEYS_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/StringRef.h"

namespace swift {
/// Define these string constants for reference dependencies (a.k.a. swiftdeps)
/// in one place to ensure consistency.
namespace reference_dependency_keys {
static constexpr StringLiteral providesTopLevel("provides-top-level");
static constexpr StringLiteral providesNominal("provides-nominal");
static constexpr StringLiteral providesMember("provides-member");
static constexpr StringLiteral providesDynamicLookup("provides-dynamic-lookup");

static constexpr StringLiteral dependsTopLevel("depends-top-level");
static constexpr StringLiteral dependsMember("depends-member");
static constexpr StringLiteral dependsNominal("depends-nominal");
static constexpr StringLiteral dependsDynamicLookup("depends-dynamic-lookup");
static constexpr StringLiteral dependsExternal("depends-external");

static constexpr StringLiteral interfaceHash("interface-hash");
} // end namespace reference_dependency_keys
} // end namespace swift

#endif
