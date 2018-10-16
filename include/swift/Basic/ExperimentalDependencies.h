//===--- ExperimentalDependencies.h - Keys for swiftdeps files --*- C++ -*-===//
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

#ifndef ExperimentalDependencies_h
#define ExperimentalDependencies_h

namespace swift {
/// Experimental dependencies evolve Swift towards finer-grained dependencies
/// and faster incremental rebuilds.

namespace ExperimentalDependencies {
// Use blank since it is illegal in Swift ids
const char separator = ' ';

std::string combineNames(StringRef a, StringRef b) {
  assert(!a.contains(separator));
  return a.str() + " " + b.str();
}
std::pair<StringRef, StringRef> separateNames(StringRef s) {
  const size_t sepIndex = s.find(separator);
  assert(sepIndex != StringRef::npos);
  return std::make_pair(s.take_front(sepIndex), s.drop_front(sepIndex + 1));
}
} // namespace ExperimentalDependencies

} // end namespace swift

#endif /* ExperimentalDependencies_h */
