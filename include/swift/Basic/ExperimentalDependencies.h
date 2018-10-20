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
#include "swift/Basic/LLVM.h"

namespace swift {
/// Experimental dependencies evolve Swift towards finer-grained dependencies
/// and faster incremental rebuilds.
  
  class Decl;

namespace ExperimentalDependencies {

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
  separate(StringRef combined);
};

  typedef const char*  unimpLocation_t;

  
  // if updateExpDepHash(Inner) is unimplemented, return where it was unimplemented
# define ExpDepQ1(a) #a
# define ExpDepQ2(a) ExpDepQ1(a)
# define RETURN_UNIMP return __FILE__ ":" ExpDepQ2(__LINE__)
  
# define TRY_UPDATE_HASH(what) \
if (ExperimentalDependencies::unimpLocation_t r  = (what)) \
  return r;

  
//qqq  void updateExpDepFromBits(llvm::MD5 &hash, const void *bits, size_t size);
  
  unimpLocation_t updateExpDepDeclHash(llvm::MD5&, const Decl*);

} // namespace ExperimentalDependencies

} // end namespace swift

#endif /* ExperimentalDependencies_h */
