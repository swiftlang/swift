//===--- DictionaryKeys.h - -------------------------------------*- C++ -*-===//
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

#ifndef LLVM_SOURCEKITD_LIB_API_DICTIONARYKEYS_H
#define LLVM_SOURCEKITD_LIB_API_DICTIONARYKEYS_H

namespace SourceKit {
  class UIdent;
}

namespace sourcekitd {

#define KEY(NAME, CONTENT) extern SourceKit::UIdent Key##NAME;
#include "SourceKit/Core/ProtocolUIDs.def"

/// Used for determining the printing order of dictionary keys.
bool compareDictKeys(SourceKit::UIdent LHS, SourceKit::UIdent RHS);

}

#endif
