//===--- Exclusivity.cpp --------------------------------------------------===//
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

#include "swift/Runtime/Exclusivity.h"
#include "swift/Runtime/Metadata.h"
#include "gtest/gtest.h"

using namespace swift;

#ifndef NDEBUG
TEST(TestExclusivity, testNullPC) {
  ValueBuffer scratch, scratch2;
  long var;
  swift_beginAccess(&var, &scratch,
                    ExclusivityFlags::WarningOnly | ExclusivityFlags::Read,
                    /*pc=*/0);
  swift_beginAccess(&var, &scratch2,
                    ExclusivityFlags::WarningOnly | ExclusivityFlags::Modify,
                    /*pc=*/0);
  swift_endAccess(&scratch2);
  swift_endAccess(&scratch);
}
#endif

#ifndef NDEBUG
TEST(TestExclusivity, testPCOne) {
  ValueBuffer scratch, scratch2;
  long var;
  swift_beginAccess(&var, &scratch,
                    ExclusivityFlags::WarningOnly | ExclusivityFlags::Read,
                    /*pc=*/(void *)1);
  swift_beginAccess(&var, &scratch2,
                    ExclusivityFlags::WarningOnly | ExclusivityFlags::Modify,
                    /*pc=*/(void *)1);
  swift_endAccess(&scratch2);
  swift_endAccess(&scratch);
}
#endif

#ifndef NDEBUG
TEST(TestExclusivity, testBogusPC) {
  ValueBuffer scratch, scratch2;
  long var;
  swift_beginAccess(&var, &scratch,
                    ExclusivityFlags::WarningOnly | ExclusivityFlags::Read,
                    /*pc=*/(void *)0xdeadbeefdeadbeefULL);
  swift_beginAccess(&var, &scratch2,
                    ExclusivityFlags::WarningOnly | ExclusivityFlags::Modify,
                    /*pc=*/(void *)0xdeadbeefdeadbeefULL);
  swift_endAccess(&scratch2);
  swift_endAccess(&scratch);
}
#endif

// rdar://32866493
TEST(TestExclusivity, testNonNested) {
  const int N = 5;
  ValueBuffer scratches[N];
  long vars[N];

  auto begin = [&](unsigned i) {
    assert(i < N);
    swift_beginAccess(&vars[i], &scratches[i], ExclusivityFlags::Modify, 0);
  };
  auto end = [&](unsigned i) {
    assert(i < N);
    swift_endAccess(&scratches[i]);
    memset(&scratches[i], /*gibberish*/ 0x99, sizeof(ValueBuffer));
  };
  auto accessAll = [&] {
    for (unsigned i = 0; i != N; ++i) begin(i);
    for (unsigned i = 0; i != N; ++i) end(i);
  };

  accessAll();
  begin(0); begin(1); end(0); end(1);
  begin(0); begin(1); end(0); end(1);
  accessAll();
  begin(1); begin(0); begin(2); end(0); end(2); end(1);
  accessAll();
  begin(0); begin(1); begin(2); begin(3); begin(4);
  end(1); end(4); end(0); end(2); end(3);
  accessAll();
}
