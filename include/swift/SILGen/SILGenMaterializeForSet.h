//===- SILGenMaterializeForSet.h - SILGen for materializeForSet -*- C++ -*-===//
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

#include <string>

namespace swift {
class ProtocolConformance;
class FuncDecl;

namespace Lowering {
/// \brief Compute the name of the callback inside an auto-generated
/// materializeForSet accessor.
///
/// FIXME: this should just be a static function inside
/// SILGenMaterializeForSet.cpp, but currently these closures end up public,
/// so TBDGen wants to emit them.
std::string getMaterializeForSetCallbackName(ProtocolConformance *conformance,
                                             FuncDecl *requirement);
}
}
