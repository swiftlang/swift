//===--- TypeLookupError.cpp - TypeLookupError Tests ----------------------===//
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

#include "swift/Demangling/TypeLookupError.h"
#include "gtest/gtest.h"
#include <vector>

using namespace swift;

TEST(TypeLookupError, ConstantString) {
  auto error = TypeLookupError("testing testing");
  char *str = error.copyErrorString();
  ASSERT_STREQ(str, "testing testing");
  error.freeErrorString(str);
}

TEST(TypeLookupError, FormatString) {
  auto error = TypeLookupError("%d %d %d %d %d %d %d %d %d %d", 0, 1, 2, 3, 4,
                               5, 6, 7, 8, 9, 10);
  char *str = error.copyErrorString();
  ASSERT_STREQ(str, "0 1 2 3 4 5 6 7 8 9");
  error.freeErrorString(str);
}

TEST(TypeLookupError, Copying) {
  std::vector<TypeLookupError> vec;

  {
    auto originalError = TypeLookupError("%d %d %d %d %d %d %d %d %d %d", 0, 1,
                                         2, 3, 4, 5, 6, 7, 8, 9, 10);
    for (int i = 0; i < 5; i++)
      vec.push_back(originalError);
  }

  for (auto &error : vec) {
    char *str = error.copyErrorString();
    ASSERT_STREQ(str, "0 1 2 3 4 5 6 7 8 9");
    error.freeErrorString(str);
  }

  auto extractedError = vec[4];
  vec.clear();
  char *str = extractedError.copyErrorString();
  ASSERT_STREQ(str, "0 1 2 3 4 5 6 7 8 9");
  extractedError.freeErrorString(str);
}
