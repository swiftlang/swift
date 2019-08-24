//===--- PathRemapper.h - Transforms path prefixes --------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file defines a data structure that stores a string-to-string
//  mapping used to transform file paths based on a prefix mapping. It
//  is optimized for the common case, which is that there will be
//  extremely few mappings (i.e., one or two).
//
//  Remappings are stored such that they are applied in the order they
//  are passed on the command line. This would only matter if one
//  source mapping was a prefix of another.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_PATHREMAPPER_H
#define SWIFT_BASIC_PATHREMAPPER_H

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Twine.h"

#include <string>
#include <utility>

namespace swift {

class PathRemapper {
  SmallVector<std::pair<std::string, std::string>, 2> PathMappings;

public:
  /// Adds a mapping such that any paths starting with `FromPrefix` have that
  /// portion replaced with `ToPrefix`.
  void addMapping(StringRef FromPrefix, StringRef ToPrefix) {
    PathMappings.emplace_back(FromPrefix, ToPrefix);
  }

  /// Returns a remapped `Path` if it starts with a prefix in the map; otherwise
  /// the original path is returned.
  std::string remapPath(StringRef Path) const {
    // Clang's implementation of this feature also compares the path string
    // directly instead of treating path segments as indivisible units. The
    // latter would arguably be more accurate, but we choose to preserve
    // compatibility with Clang (especially because we propagate the flag to
    // ClangImporter as well).
    for (const auto &Mapping : PathMappings)
      if (Path.startswith(Mapping.first))
        return (Twine(Mapping.second) +
                Path.substr(Mapping.first.size())).str();
    return Path.str();
  }
};

} // end namespace swift

#endif // SWIFT_BASIC_PATHREMAPPER_H
