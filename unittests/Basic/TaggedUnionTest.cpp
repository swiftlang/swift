//===--- TaggedUnionTest.cpp ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/TaggedUnion.h"
#include "gtest/gtest.h"
#include <vector>
#include <string.h>

using namespace swift;

TEST(TaggedUnion, basics_trivial) {
  using UnionType = TaggedUnion<int, long>;
  UnionType u1 = 21;
  EXPECT_FALSE(u1.isa<long>());
  EXPECT_FALSE(u1.dyn_cast<long>() != nullptr);
  EXPECT_TRUE(u1.isa<int>());
  EXPECT_TRUE(u1.dyn_cast<int>() != nullptr);
  EXPECT_EQ(21, u1.get<int>());
  EXPECT_EQ(21, *u1.dyn_cast<int>());

  UnionType u2 = 14L;
  EXPECT_FALSE(u2.isa<int>());
  EXPECT_FALSE(u2.dyn_cast<int>() != nullptr);
  EXPECT_TRUE(u2.isa<long>());
  EXPECT_TRUE(u2.dyn_cast<long>() != nullptr);
  EXPECT_EQ(14L, u2.get<long>());
  EXPECT_EQ(14L, *u2.dyn_cast<long>());

  UnionType u3 = u1;
  EXPECT_FALSE(u3.isa<long>());
  EXPECT_FALSE(u3.dyn_cast<long>() != nullptr);
  EXPECT_TRUE(u3.isa<int>());
  EXPECT_TRUE(u3.dyn_cast<int>() != nullptr);
  EXPECT_EQ(21, u3.get<int>());
  EXPECT_EQ(21, *u3.dyn_cast<int>());

  UnionType u4 = u2;
  EXPECT_FALSE(u4.isa<int>());
  EXPECT_FALSE(u4.dyn_cast<int>() != nullptr);
  EXPECT_TRUE(u4.isa<long>());
  EXPECT_TRUE(u4.dyn_cast<long>() != nullptr);
  EXPECT_EQ(14L, u4.get<long>());
  EXPECT_EQ(14L, *u4.dyn_cast<long>());

  long twenty_nine_ell = 29L;
  u4 = twenty_nine_ell;
  EXPECT_FALSE(u4.isa<int>());
  EXPECT_FALSE(u4.dyn_cast<int>() != nullptr);
  EXPECT_TRUE(u4.isa<long>());
  EXPECT_TRUE(u4.dyn_cast<long>() != nullptr);
  EXPECT_EQ(29L, u4.get<long>());
  EXPECT_EQ(29L, *u4.dyn_cast<long>());

  u4 = u1;
  EXPECT_FALSE(u4.isa<long>());
  EXPECT_FALSE(u4.dyn_cast<long>() != nullptr);
  EXPECT_TRUE(u4.isa<int>());
  EXPECT_TRUE(u4.dyn_cast<int>() != nullptr);
  EXPECT_EQ(21, u4.get<int>());
  EXPECT_EQ(21, *u4.dyn_cast<int>());

  u4 = 71L;
  EXPECT_FALSE(u4.isa<int>());
  EXPECT_FALSE(u4.dyn_cast<int>() != nullptr);
  EXPECT_TRUE(u4.isa<long>());
  EXPECT_TRUE(u4.dyn_cast<long>() != nullptr);
  EXPECT_EQ(71L, u4.get<long>());
  EXPECT_EQ(71L, *u4.dyn_cast<long>());
}

TEST(TaggedUnion, void_basics) {
  using UnionType = TaggedUnion<int, long, void>;

  UnionType u1;
  EXPECT_FALSE(u1.isa<long>());
  EXPECT_FALSE(u1.isa<int>());
  EXPECT_TRUE(u1.isa<void>());
  EXPECT_TRUE(u1.empty());
  EXPECT_TRUE(u1.dyn_cast<int>() == nullptr);

  u1.emplace<int>(111);
  EXPECT_FALSE(u1.isa<long>());
  EXPECT_TRUE(u1.isa<int>());
  EXPECT_FALSE(u1.isa<void>());
  EXPECT_FALSE(u1.empty());
  EXPECT_EQ(111, u1.get<int>());

  u1.reset();
  EXPECT_FALSE(u1.isa<long>());
  EXPECT_FALSE(u1.isa<int>());
  EXPECT_TRUE(u1.isa<void>());
  EXPECT_TRUE(u1.empty());
  EXPECT_TRUE(u1.dyn_cast<int>() == nullptr);
}

struct OptionalString {
  const char *Value;

  OptionalString(const char *value = nullptr) : Value(value) {}
  OptionalString(const OptionalString &other) : Value(other.Value) {}
  OptionalString(OptionalString &&other) : Value(other.Value) {
    other.Value = nullptr;
  }
  OptionalString &operator=(const OptionalString &other) {
    Value = other.Value;
    return *this;
  }
  OptionalString &operator=(OptionalString &&other) {
    Value = other.Value;
    other.Value = nullptr;
    return *this;
  }
  ~OptionalString() {}

  bool empty() const { return Value == nullptr; }
  explicit operator bool() const { return empty(); }

  friend bool operator==(OptionalString lhs, OptionalString rhs) {
    return lhs.Value == rhs.Value ||
          (lhs && rhs && strcmp(lhs.Value, rhs.Value) == 0);
  }
  friend bool operator!=(OptionalString lhs, OptionalString rhs) {
    return !(lhs == rhs);
  }
};

TEST(TaggedUnion, nontrivial_basics) {
  using Value = OptionalString;
  using ValueVector = std::vector<Value>;
  using UnionType = TaggedUnion<Value, ValueVector>;

  Value str = "hello";
  UnionType u1 = str;
  EXPECT_FALSE(u1.isa<ValueVector>());
  EXPECT_FALSE(u1.dyn_cast<ValueVector>() != nullptr);
  EXPECT_TRUE(u1.isa<Value>());
  EXPECT_TRUE(u1.dyn_cast<Value>() != nullptr);
  EXPECT_EQ("hello", u1.get<Value>());
  EXPECT_EQ("hello", *u1.dyn_cast<Value>());

  str = "world";
  EXPECT_EQ("hello", u1.get<Value>());
  EXPECT_EQ("hello", *u1.dyn_cast<Value>());

  ValueVector vec;
  vec.push_back("a");
  vec.push_back("b");
  vec.push_back("c");

  UnionType u2 = vec;
  EXPECT_FALSE(u2.isa<Value>());
  EXPECT_FALSE(u2.dyn_cast<Value>() != nullptr);
  EXPECT_TRUE(u2.isa<ValueVector>());
  EXPECT_TRUE(u2.dyn_cast<ValueVector>() != nullptr);
  EXPECT_EQ("b", u2.get<ValueVector>()[1]);

  UnionType u3 = u2;
  EXPECT_FALSE(u3.isa<Value>());
  EXPECT_FALSE(u3.dyn_cast<Value>() != nullptr);
  EXPECT_TRUE(u3.isa<ValueVector>());
  EXPECT_TRUE(u3.dyn_cast<ValueVector>() != nullptr);
  EXPECT_EQ("b", u3.get<ValueVector>()[1]);

  u3 = u1;
  EXPECT_FALSE(u3.isa<ValueVector>());
  EXPECT_FALSE(u3.dyn_cast<ValueVector>() != nullptr);
  EXPECT_TRUE(u3.isa<Value>());
  EXPECT_TRUE(u3.dyn_cast<Value>() != nullptr);
  EXPECT_EQ("hello", u3.get<Value>());
  EXPECT_EQ("hello", *u3.dyn_cast<Value>());

  u3 = u2;
  EXPECT_FALSE(u3.isa<Value>());
  EXPECT_FALSE(u3.dyn_cast<Value>() != nullptr);
  EXPECT_TRUE(u3.isa<ValueVector>());
  EXPECT_TRUE(u3.dyn_cast<ValueVector>() != nullptr);
  EXPECT_EQ("a", u3.get<ValueVector>()[0]);

  u3 = std::move(u1);
  EXPECT_FALSE(u3.isa<ValueVector>());
  EXPECT_FALSE(u3.dyn_cast<ValueVector>() != nullptr);
  EXPECT_TRUE(u3.isa<Value>());
  EXPECT_TRUE(u3.dyn_cast<Value>() != nullptr);
  EXPECT_EQ("hello", u3.get<Value>());
  EXPECT_EQ("hello", *u3.dyn_cast<Value>());

  // These will still be true, but the value will have been moved out of.
  EXPECT_FALSE(u1.isa<ValueVector>());
  EXPECT_FALSE(u1.isa<ValueVector>());
  EXPECT_TRUE(u1.isa<Value>());
  EXPECT_TRUE(u1.dyn_cast<Value>() != nullptr);  
  EXPECT_EQ(Value(), u1.get<Value>());
  EXPECT_EQ(Value(), *u1.dyn_cast<Value>());

  UnionType u4 = std::move(u2);
  EXPECT_FALSE(u4.isa<Value>());
  EXPECT_FALSE(u4.dyn_cast<Value>() != nullptr);
  EXPECT_TRUE(u4.isa<ValueVector>());
  EXPECT_TRUE(u4.dyn_cast<ValueVector>() != nullptr);
  EXPECT_EQ(size_t(3), u4.get<ValueVector>().size());
  EXPECT_EQ("a", u4.get<ValueVector>()[0]);

  // These will still be true, but the value will have been moved out of.
  EXPECT_FALSE(u2.isa<Value>());
  EXPECT_FALSE(u2.dyn_cast<Value>() != nullptr);
  EXPECT_TRUE(u2.isa<ValueVector>());
  EXPECT_TRUE(u2.dyn_cast<ValueVector>() != nullptr);
  EXPECT_TRUE(u2.get<ValueVector>().empty());
  EXPECT_TRUE(u2.dyn_cast<ValueVector>()->empty());
}
