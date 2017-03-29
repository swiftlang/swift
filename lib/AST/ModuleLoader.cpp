//===--- ModuleLoader.cpp - Swift Language Module Implementation ----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements the ModuleLoader class and/or any helpers.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ModuleLoader.h"
#include "clang/Frontend/Utils.h"
#include "swift/ClangImporter/ClangImporter.h"

namespace swift {

DependencyTracker::DependencyTracker()
  // NB: The ClangImporter believes it's responsible for the construction of
  // this instance, and it static_cast<>s the instance pointer to its own
  // subclass based on that belief. If you change this to be some other
  // instance, you will need to change ClangImporter's code to handle the
  // difference.
  : clangCollector(ClangImporter::createDependencyCollector())
{
}

void
DependencyTracker::addDependency(StringRef File, bool IsSystem) {
  // DependencyTracker exposes an interface that (intentionally) does not talk
  // about clang at all, nor about missing deps. It does expose an IsSystem
  // dimension, though it is presently always false, we accept it and pass it
  // along to the clang DependencyCollector in case Swift callers start setting
  // it to true someday.
  clangCollector->maybeAddDependency(File, /*FromClangModule=*/false,
                                     IsSystem, /*IsClangModuleFile=*/false,
                                     /*IsMissing=*/false);
}

ArrayRef<std::string>
DependencyTracker::getDependencies() const {
  return clangCollector->getDependencies();
}

std::shared_ptr<clang::DependencyCollector>
DependencyTracker::getClangCollector() {
  return clangCollector;
}

}
