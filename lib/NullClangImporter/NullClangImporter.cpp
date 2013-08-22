//===--- NullClangImporter.cpp --------------------------------------------===//
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
// This file implements swift::getClangImporterCtor() as returning null.
// This is so clients that don't want to link in clang don't have to,
// they should link NullClangImporter.
//
//===----------------------------------------------------------------------===//

#include "swift/ClangImporter/ClangImporter.h"

using namespace swift;

ClangImporterCtorTy swift::getClangImporterCtor() {
  return nullptr;
}
