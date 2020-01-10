//===--- JSON.h - Symbol Graph JSON Helpers -------------------------------===//
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
// Adds Symbol Graph JSON serialization to other types.
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SYMBOLGRAPHGEN_JSON_H
#define SWIFT_SYMBOLGRAPHGEN_JSON_H

#include "llvm/ADT/Triple.h"
#include "llvm/Support/JSON.h"
#include "llvm/Support/VersionTuple.h"
#include "swift/AST/GenericSignature.h"

namespace swift {
namespace symbolgraphgen {

struct AttributeRAII {
  StringRef Key;
  llvm::json::OStream &OS;
  AttributeRAII(StringRef Key, llvm::json::OStream &OS)
  : Key(Key), OS(OS) {
    OS.attributeBegin(Key);
  }

  ~AttributeRAII() {
    OS.attributeEnd();
  }
};

void serialize(const llvm::VersionTuple &VT, llvm::json::OStream &OS);
void serialize(const llvm::Triple &T, llvm::json::OStream &OS);

} // end namespace symbolgraphgen
} // end namespace swift

#endif // SWIFT_SYMBOLGRAPHGEN_JSON_H
