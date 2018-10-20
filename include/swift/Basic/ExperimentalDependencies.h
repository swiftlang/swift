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

#include "llvm/Support/MD5.h"

namespace swift {
/// Experimental dependencies evolve Swift towards finer-grained dependencies
/// and faster incremental rebuilds.

namespace ExperimentalDependencies {
//// Use blank since it is illegal in Swift ids
//const char separator = ' ';
//
//struct Utils {
//  static std::string combineNames(std::string a, std::string b) {
//    assert(a.find(separator) == std::string::npos);
//    return a + std::string(1, separator) + b;
//  }
//  static std::pair<StringRef, StringRef> separateNames(StringRef s) {
//    const size_t sepIndex = s.find(separator);
//    if (sepIndex == StringRef::npos) {
//      return std::make_pair(s, StringRef());
//    }
//    return std::make_pair(s.take_front(sepIndex), s.drop_front(sepIndex + 1));
//  }
//};

//struct InterfaceHashes {
//  const std::string normal;
//  const std::string experimental;
//
//  InterfaceHashes(StringRef normal, StringRef experimental)
//      : normal(normal.str()), experimental(experimental.str()) {}
//  InterfaceHashes(StringRef combined)
//      : InterfaceHashes(Utils::separateNames(combined)) {}
//  InterfaceHashes(std::pair<StringRef, StringRef> ne)
//      : normal(ne.first.str()), experimental(ne.second.str()) {}
//
//  std::string combined() { return Utils::combineNames(normal, experimental); }
//};

struct CompoundProvides {
  const std::string name;
  const std::string hash;
  const std::string unimpLoc;
  
  CompoundProvides(std::string name, std::string hash, const char *unimpLoc)
  : name(name), hash(hash), unimpLoc(unimpLoc) {}

  CompoundProvides(StringRef combined) : CompoundProvides(separate(combined)) {}
  
  CompoundProvides(std::tuple<std::string, std::string, std::string> separated) :
  name(std::get<0>(separated)), hash(std::get<1>(separated)), unimpLoc(std::get<2>(separated)) {}
 
 

  std::string combined() {
    // no spaces in Swift names, assume hashes don't start with ###
    return std::string(name) + nameTrailer() + (!unimpLoc.empty() ? unimpLoc : hashPrefix() + hash);
  }

private:
  
  static char nameTrailer() { return ' '; }
  static const char *hashPrefix() { return  "###"; }
  
  static std::tuple<std::string, std::string, std::string>
  separate(StringRef combined) {
    std::string name;
    std::string hash;
    std::string unimpLoc;
    
    const size_t sepIndex = combined.find(nameTrailer());
    if (sepIndex == StringRef::npos) {
      name = combined;
    }
    else {
      name = combined.take_front(sepIndex);
      const std::string hashOrUnimpLoc = combined.drop_front(sepIndex + 1);
      if (StringRef(hashOrUnimpLoc).startswith(hashPrefix()))
        hash = hashOrUnimpLoc.substr(strlen(hashPrefix()));
      else
        unimpLoc = hashOrUnimpLoc;
    }
    return std::make_tuple(name, hash, unimpLoc);
  }
};

  typedef const char*  unimpLocation_t;

  
  // if updateExpDepHash(Inner) is unimplemented, return where it was unimplemented
# define ExpDepQ1(a) #a
# define ExpDepQ2(a) ExpDepQ1(a)
# define RETURN_UNIMP return __FILE__ ":" ExpDepQ2(__LINE__)
  
# define TRY_UPDATE_HASH(what) \
if (ExperimentalDependencies::unimpLocation_t r  = (what)) \
  return r;

  
  void updateExpDepFromBits(llvm::MD5 &hash, const void *bits, size_t size);

} // namespace ExperimentalDependencies

} // end namespace swift


#endif /* ExperimentalDependencies_h */
