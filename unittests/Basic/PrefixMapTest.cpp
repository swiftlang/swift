//===--- PrefixMapTest.cpp ------------------------------------------------===//
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

#include "swift/Basic/PrefixMap.h"
#include "gtest/gtest.h"

#include "llvm/ADT/SmallString.h"

#include <string>
#include <map>

using namespace swift;

static ArrayRef<char> asArray(StringRef str) {
  return ArrayRef<char>(str.begin(), str.end());
}
static StringRef asString(ArrayRef<char> str) {
  return StringRef(str.begin(), str.size());
}

namespace {

struct Tester {
  std::map<std::string, int> StdMap;
  PrefixMap<char, int> PreMap;

  Tester() {
    assert(PreMap.empty());
  }

  // Performs an insert operation and tests that the result matches what
  // we get from the std::map.
  void insert(StringRef key, int value) {
    auto stdmapResult = StdMap.insert({key, value});
    auto premapResult = PreMap.insert(asArray(key), value);

    // Whether or not we modified the map should be the same in both
    // implementations.
    EXPECT_EQ(stdmapResult.second, premapResult.second);

    // If we did insert a new entry, the value at the handle should
    // reflect the value we inserted.
    if (premapResult.second) {
      EXPECT_EQ(value, *premapResult.first);
    }
  }

  // Tests that the result of a findPrefix matches what we expect from
  // the std::map.
  void find(StringRef key) {
    auto stdmapResult = StdMap.lower_bound(key);
    while (stdmapResult == StdMap.end() ||
           !key.startswith(stdmapResult->first)) {
      if (stdmapResult == StdMap.begin()) {
        stdmapResult = StdMap.end();
        break;
      }
      --stdmapResult;
    }
    bool hasStdmapEntry = (stdmapResult != StdMap.end());

    auto premapResult = PreMap.findPrefix(asArray(key));

    EXPECT_EQ(hasStdmapEntry, bool(premapResult.first));
    if (!hasStdmapEntry) return;

    assert(key.startswith(stdmapResult->first));
    auto stdmapValue = stdmapResult->second;
    EXPECT_EQ(stdmapValue, *premapResult.first);
    EXPECT_EQ(key.begin() + stdmapResult->first.size(), premapResult.second);
  }

  // Perform a clear operation.  Tests that that actually clears out the map.
  void clear() {
    StdMap.clear();
    PreMap.clear();
    EXPECT_TRUE(PreMap.empty());
    EXPECT_EQ(0U, PreMap.size());
  }

  // Validate the map.  Primarily tests iteration.
  void validate() {
    EXPECT_EQ(StdMap.empty(), PreMap.empty());
    EXPECT_EQ(StdMap.size(), PreMap.size());

    auto si = StdMap.begin(), se = StdMap.end();
    auto pi = PreMap.begin(), pe = PreMap.end();
    while (true) {
      if (si == se) {
        EXPECT_TRUE(pi == pe);
        return;
      }
      EXPECT_TRUE(pi != pe);

      EXPECT_EQ(si->second, (*pi).getValue());

      llvm::SmallString<128> buffer;
      EXPECT_EQ(StringRef(si->first), asString((*pi).getKey(buffer)));

      ++si;
      ++pi;
    }
  }
};

} // end anonymous namespace

TEST(PrefixMapTest, Insert) {
  PrefixMap<char, int> map;
  map.insert(asArray("abcdefghi"), 5);
  EXPECT_FALSE(bool(map.findPrefix(asArray("abcdefg")).first));

  ArrayRef<char> abcdefghij = asArray("abcdefghij");
  auto abcdefghijHandle = map.findPrefix(abcdefghij);
  EXPECT_TRUE(bool(abcdefghijHandle.first));
  EXPECT_EQ(5, *abcdefghijHandle.first);
  EXPECT_EQ(abcdefghij.data() + 9, abcdefghijHandle.second);
}

TEST(PrefixMapTest, Test1) {
  Tester tester;
  tester.find("");
  tester.validate();
  tester.insert("", 22096);
  tester.validate();
  tester.insert("", 20154);
  tester.validate();
  tester.insert("aezvs", 21441);
  tester.validate();
  tester.insert("aezvs", 838);
  tester.validate();
  tester.insert("aezvs", 16768);
  tester.validate();
  tester.validate();
  tester.find("aezvsrmoll");
  tester.validate();
  tester.insert("aezvsrmoll", 5842);
  tester.validate();
  tester.find("aezvsrmoll");
  tester.validate();
}

TEST(PrefixMapTest, Test2) {
  Tester tester;
  tester.find("");
  tester.validate();
  tester.insert("", 22096);
  tester.validate();
  tester.insert("", 20154);
  tester.validate();
  tester.insert("aezvs", 21441);
  tester.validate();
  tester.insert("aezvs", 838);
  tester.validate();
  tester.insert("aezvs", 16768);
  tester.validate();
  tester.clear();
  tester.validate();
  tester.find("aezvsrmoll");
  tester.validate();
  tester.insert("aezvsrmoll", 5842);
  tester.validate();
  tester.find("aezvsrmoll");
  tester.validate();
}

TEST(PrefixMapTest, Test3) {
  Tester tester;
  tester.insert("wbuqbtaprrpooqteftzdhjd", 11861);
  tester.insert("wbuqbtaprrpooqteftzdhjd", 18531);
  tester.find("wbuqbtaprrpooqteftzdhjdqkemtcl");
}

TEST(PrefixMapTest, Test4) {
  Tester tester;
  tester.find("");
  tester.validate();
  tester.find("l");
  tester.validate();
  tester.insert("l", 5943);
  tester.validate();
  tester.find("l");
  tester.validate();
  tester.insert("l", 8632);
  tester.validate();
  tester.find("zguowwnctgmkg");
}
