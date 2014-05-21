//===- DependencyFileGenerator.cpp - .d file generation -------------------===//
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

#include "swift/Frontend/DependencyFileGenerator.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

void DependencyFileGenerator::addDependency(StringRef file) {
  paths.insert(file);
}

void DependencyFileGenerator::addTarget(StringRef targetName) {
  if (!targetName.empty())
    targets.push_back(targetName);
}

void DependencyFileGenerator::writeToStream(llvm::raw_ostream &os) const {
  // Declare a helper for escaping file names for use in Makefiles.
  llvm::SmallString<256> pathBuf;
  auto escape = [&](StringRef raw) -> StringRef {
    pathBuf.clear();
    
    static const char badChars[] = " $#:\n";
    size_t prev = 0;
    for (auto index = raw.find_first_of(badChars); index != StringRef::npos;
         index = raw.find_first_of(badChars, index+1)) {
      pathBuf.append(raw.slice(prev, index));
      if (raw[index] == '$')
        pathBuf.push_back('$');
      else
        pathBuf.push_back('\\');
      prev = index;
    }
    pathBuf.append(raw.substr(prev));
    return pathBuf;
  };

  // FIXME: Xcode can't currently handle multiple targets in a single
  // dependency line.
  for (StringRef targetName : targets) {
    os << targetName << " :";
    for (StringRef path : paths)
      os << ' ' << escape(path);
    os << '\n';
  }
}
