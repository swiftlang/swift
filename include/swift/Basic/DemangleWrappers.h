//===--- DemangleWrappers.h -------------------------------------*- C++ -*-===//
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
///
/// \file
/// This file defines wrappers for the Swift demangler that use LLVM data
/// types.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_DEMANGLEWRAPPERS_H
#define SWIFT_BASIC_DEMANGLEWRAPPERS_H

#include "swift/Basic/Demangle.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/StringRef.h"

namespace swift {
namespace demangle_wrappers {

using swift::Demangle::Node;
using swift::Demangle::NodePointer;
using swift::Demangle::DemangleOptions;

class NodeDumper {
  NodePointer Root;

public:
  NodeDumper(NodePointer Root): Root(std::move(Root)) {}
  void dump() const;
  void print(llvm::raw_ostream &Out) const;
};

/// Utility function, useful to be called from the debugger.
void dumpNode(const NodePointer &Root);

NodePointer
demangleSymbolAsNode(StringRef MangledName,
                     const DemangleOptions &Options = DemangleOptions());

std::string nodeToString(NodePointer Root,
                         const DemangleOptions &Options = DemangleOptions());

std::string
demangleSymbolAsString(StringRef MangledName,
                       const DemangleOptions &Options = DemangleOptions());

std::string
demangleTypeAsString(StringRef MangledTypeName,
                     const DemangleOptions &Options = DemangleOptions());

} // end namespace demangle_wrappers
} // end namespace swift

#endif // SWIFT_BASIC_DEMANGLEWRAPPERS_H
