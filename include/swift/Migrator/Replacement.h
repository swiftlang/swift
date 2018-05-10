//===--- Replacement.h - Migrator Replacements ------------------*- C++ -*-===//
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

#ifndef SWIFT_MIGRATOR_REPLACEMENT_H
#define SWIFT_MIGRATOR_REPLACEMENT_H
namespace swift {
namespace migrator {

struct Replacement {
  size_t Offset;
  size_t Remove;
  std::string Text;

  bool isRemove() const {
    return Remove > 0;
  }

  bool isInsert() const { return Remove == 0 && !Text.empty(); }

  bool isReplace() const { return Remove > 0 && !Text.empty(); }

  size_t endOffset() const {
    if (isInsert()) {
      return Offset + Text.size();
    } else {
      return Offset + Remove;
    }
  }

  bool operator<(const Replacement &Other) const {
    return Offset < Other.Offset;
  }

  bool operator==(const Replacement &Other) const {
    return Offset == Other.Offset && Remove == Other.Remove &&
      Text == Other.Text;
  }
};

} // end namespace migrator
} // end namespace swift

#endif // SWIFT_MIGRATOR_REPLACEMENT_H
