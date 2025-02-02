//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_LOADDYNAMICLIBRARY_H
#define SWIFT_BASIC_LOADDYNAMICLIBRARY_H

#include <string>

namespace swift {
void *loadLibrary(const char *path, std::string *err);
void *getAddressOfSymbol(void *handle, const char *symbol);
} // end namespace swift

#endif // SWIFT_BASIC_LOADDYNAMICLIBRARY_H
