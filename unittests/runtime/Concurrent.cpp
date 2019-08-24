//===--- Concurrent.cpp - Concurrent data structure tests -----------------===//
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

#include "swift/Runtime/Concurrent.h"
#include "gtest/gtest.h"

using namespace swift;

TEST(ConcurrentReadableArrayTest, SingleThreaded) {
  ConcurrentReadableArray<size_t> array;
  
  auto add = [&](size_t limit) {
    for (size_t i = array.snapshot().count(); i < limit; i++)
      array.push_back(i);
  };
  auto check = [&]{
    size_t i = 0;
    for (auto element : array.snapshot()) {
      ASSERT_EQ(element, i);
      i++;
    }
  };
  
  check();
  add(1);
  check();
  add(16);
  check();
  add(100);
  check();
  add(1000);
  check();
  add(1000000);
  check();
}
