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

TEST(TextExclusivity, testNullPC) {
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

TEST(TextExclusivity, testPCOne) {
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

TEST(TextExclusivity, testBogusPC) {
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
