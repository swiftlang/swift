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
  class NominalTypeDecl;
  class ValueDecl;

namespace ExperimentalDependencies {
  
  // could bring in the thing in DependencyGraph
  enum class ProvidesKind {
    topLevel, nominal, dynamicLookup, member
  };
  
  /// Encode whether we have a hash or unimplemented location by prefixing the hash with ###
  class HashOrUnimpLoc {
    static const char *hashPrefix() { return  "###"; }

  public:
    const std::string hash;
    const std::string unimpLoc;
    
    bool hasHash() const { return !hash.empty(); }
    
    std::string combined() const {
      return hasHash()
      ? std::string(hashPrefix()) + hash
      : unimpLoc;
    }
    
    
    explicit HashOrUnimpLoc(StringRef combined) : HashOrUnimpLoc(separate(combined)) {}
    
  private:
    static std::pair<std::string, std::string>
    separate(StringRef combined) {
      return combined.startswith(hashPrefix())
      ? std::make_pair(
                       combined.drop_front(strlen(hashPrefix())).str(),
                       std::string()
                       )
      : std::make_pair(std::string(), combined.str());
    }
    
    HashOrUnimpLoc(std::pair<std::string, std::string> both) :
    HashOrUnimpLoc(both.first, both.second) {}
  public:
    
    static HashOrUnimpLoc forHash(StringRef hash) {
      return HashOrUnimpLoc(hash, std::string());
    }
    static HashOrUnimpLoc forUnimpLoc(StringRef unimpLoc) {
      return HashOrUnimpLoc(std::string(), unimpLoc.str());
    }
    
    HashOrUnimpLoc(StringRef hash, StringRef unimpLoc) :
    hash(hash.str()), unimpLoc(unimpLoc) {}
  };

class CompoundProvides {
private:
  static char nameTrailer() { return ' '; }
  
public:
  const std::string name;
  const HashOrUnimpLoc hashOrUnimpLoc;
  
  bool hasHash() const { return hashOrUnimpLoc.hasHash(); }
  std::string hash() const { return hashOrUnimpLoc.hash; }
  
  std::string combined() {
    return name + nameTrailer() + hashOrUnimpLoc.combined();
  }

  
  
  CompoundProvides(std::string name, std::string hash, std::string unimpLoc)
  : CompoundProvides(name, HashOrUnimpLoc(hash, unimpLoc)) {}
  
  CompoundProvides(std::string name, HashOrUnimpLoc hashOrUnimpLoc)
  : name(verifyName(name)), hashOrUnimpLoc(hashOrUnimpLoc) {}
  
private:
  static std::string const &verifyName(const std::string &name) {
    assert(name.find(nameTrailer()) == std::string::npos);
    return name;
  }
public:

  CompoundProvides(StringRef combined) : CompoundProvides(separate(combined)) {}
  
private:
  static std::pair<std::string, HashOrUnimpLoc>
  separate(StringRef combined) {
    const size_t sepIndex = combined.find(nameTrailer());
    if (sepIndex == StringRef::npos) {
      HashOrUnimpLoc huc = HashOrUnimpLoc::forUnimpLoc("no hash");
      return std::make_pair(combined.str(), huc);
    }
    HashOrUnimpLoc huc = HashOrUnimpLoc(combined.drop_front(sepIndex + 1));
    return std::make_pair( combined.take_front(sepIndex), huc);
  }
  
  CompoundProvides(std::pair<std::string, HashOrUnimpLoc> both) :
  name(both.first), hashOrUnimpLoc(both.second) {}
};
  
  template<ProvidesKind kind, typename DeclT>
  std::string getCombinedNameAndProvidesHash(StringRef, const DeclT*);
  
  
  typedef const char*  unimpLocation_t;

  // if updateExpDepHash(Inner) is unimplemented, return where it was unimplemented
# define ExpDepQ1(a) #a
# define ExpDepQ2(a) ExpDepQ1(a)
# define UNIMP_HASH __FILE__ ":" ExpDepQ2(__LINE__)
  
# define TRY_UPDATE_HASH(what) \
if (ExperimentalDependencies::unimpLocation_t r  = (what)) \
  return r;

  
//qqq  void updateExpDepFromBits(llvm::MD5 &hash, const void *bits, size_t size);
  
  unimpLocation_t updateExpDepDeclHash(llvm::MD5&, const Decl*);
  
  std::string scrub(StringRef);

} // namespace ExperimentalDependencies

} // end namespace swift

#endif /* ExperimentalDependencies_h */
