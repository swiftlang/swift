//===- DependencyFileGenerator.h - .d file generation -----------*- C++ -*-===//
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

#ifndef SWIFT_FRONTEND_DEPENDENCYFILEGENERATOR_H
#define SWIFT_FRONTEND_DEPENDENCYFILEGENERATOR_H

#include "swift/AST/ModuleLoader.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/StringSet.h"
#include <vector>

namespace swift {
  class DependencyFileGenerator : public DependencyTracker {
    std::vector<std::string> targets;
    llvm::SetVector<std::string, std::vector<std::string>,
                    llvm::StringSet<>> paths;
  public:
    virtual void addDependency(StringRef file) override;

    void addTarget(StringRef targetName);
    void writeToStream(llvm::raw_ostream &os) const;
  };
}

#endif
