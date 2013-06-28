// RUN: %swift -i %s | FileCheck %s
// REQUIRES: sdk

import POSIX

func test_GetoptLongA0() {
  var argv = ["a.out"]
  var opts = GetoptLongOptions()
  var parsedOpts = Vector<GetoptLong.ErrorOrOption>()
  var nonOptions = Vector<String>()
  for r in GetoptLong(argv, opts, nonOptions) {
    parsedOpts.append(r)
  }

  assert(parsedOpts.length == 0, "A0_1")

  assert(nonOptions.isEmpty, "A0_2")
}

func test_GetoptLongA1() {
  var argv = ["a.out"]
  var opts = GetoptLongOptions().noArgument("foo")
  var parsedOpts = Vector<GetoptLong.ErrorOrOption>()
  var nonOptions = Vector<String>()
  for r in GetoptLong(argv, opts, nonOptions) {
    parsedOpts.append(r)
  }

  assert(parsedOpts.length == 0, "A1_1")

  assert(nonOptions.isEmpty, "A1_2")
}

func test_GetoptLongA2() {
  var argv = ["a.out", "--foo"]
  var opts = GetoptLongOptions().noArgument("foo")
  var parsedOpts = Vector<GetoptLong.ErrorOrOption>()
  var nonOptions = Vector<String>()
  for r in GetoptLong(argv, opts, nonOptions) {
    parsedOpts.append(r)
  }

  assert(parsedOpts.length == 1, "A2_1")

  assert(!parsedOpts[0].isError, "A2_2")
  assert(parsedOpts[0].optionIndex.get() == 0, "A2_3")
  assert(parsedOpts[0].arg.isNone(), "A2_4")

  assert(nonOptions.isEmpty, "A2_5")
}

func test_GetoptLongA3() {
  var argv = ["a.out", "--bar"]
  var opts = GetoptLongOptions().noArgument("foo")
                                .noArgument("bar")
  var parsedOpts = Vector<GetoptLong.ErrorOrOption>()
  var nonOptions = Vector<String>()
  for r in GetoptLong(argv, opts, nonOptions) {
    parsedOpts.append(r)
  }

  assert(parsedOpts.length == 1, "A3_1")

  assert(!parsedOpts[0].isError, "A3_2")
  assert(parsedOpts[0].optionIndex.get() == 1, "A3_3")
  assert(parsedOpts[0].arg.isNone(), "A3_4")

  assert(nonOptions.isEmpty, "A3_5")
}

func test_GetoptLongA4() {
  var argv = ["a.out", "--bar", "--foo"]
  var opts = GetoptLongOptions().noArgument("foo")
                                .noArgument("bar")
  var parsedOpts = Vector<GetoptLong.ErrorOrOption>()
  var nonOptions = Vector<String>()
  for r in GetoptLong(argv, opts, nonOptions) {
    parsedOpts.append(r)
  }

  assert(parsedOpts.length == 2, "A4_1")

  assert(!parsedOpts[0].isError, "A4_2")
  assert(parsedOpts[0].optionIndex.get() == 1, "A4_3")
  assert(parsedOpts[0].arg.isNone(), "A4_4")

  assert(!parsedOpts[1].isError, "A4_5")
  assert(parsedOpts[1].optionIndex.get() == 0, "A4_6")
  assert(parsedOpts[1].arg.isNone(), "A4_7")

  assert(nonOptions.isEmpty, "A4_8")
}

func test_GetoptLongA5() {
  var argv = ["a.out", "--bar", "--baz"]
  var opts = GetoptLongOptions().noArgument("foo")
                                .noArgument("bar")
  var parsedOpts = Vector<GetoptLong.ErrorOrOption>()
  var nonOptions = Vector<String>()
  for r in GetoptLong(argv, opts, nonOptions) {
    parsedOpts.append(r)
  }

  assert(parsedOpts.length == 2, "A5_1")

  assert(!parsedOpts[0].isError, "A5_2")
  assert(parsedOpts[0].optionIndex.get() == 1, "A5_3")
  assert(parsedOpts[0].arg.isNone(), "A5_4")

  assert(parsedOpts[1].isUnknownOption, "A5_5")
  assert(parsedOpts[1].optionIndex.isNone(), "A5_6")
  assert(parsedOpts[1].arg.isNone(), "A5_7")

  assert(nonOptions.isEmpty, "A5_8")
}

func test_GetoptLongA6() {
  var argv = ["a.out", "--bar", "--baz"]
  var opts = GetoptLongOptions().noArgument("foo")
  var parsedOpts = Vector<GetoptLong.ErrorOrOption>()
  var nonOptions = Vector<String>()
  for r in GetoptLong(argv, opts, nonOptions) {
    parsedOpts.append(r)
  }

  assert(parsedOpts.length == 2, "A6_1")

  assert(parsedOpts[0].isUnknownOption, "A6_2")
  assert(parsedOpts[0].optionIndex.isNone(), "A6_3")
  assert(parsedOpts[0].arg.isNone(), "A6_4")

  assert(parsedOpts[1].isUnknownOption, "A6_5")
  assert(parsedOpts[1].optionIndex.isNone(), "A6_6")
  assert(parsedOpts[1].arg.isNone(), "A6_7")

  assert(nonOptions.isEmpty, "A6_8")
}

func test_GetoptLongB0() {
  var argv = ["a.out"]
  var opts = GetoptLongOptions().noArgument("foo", 'f')
  var parsedOpts = Vector<GetoptLong.ErrorOrOption>()
  var nonOptions = Vector<String>()
  for r in GetoptLong(argv, opts, nonOptions) {
    parsedOpts.append(r)
  }

  assert(parsedOpts.length == 0, "B0_1")
  assert(nonOptions.isEmpty, "B0_2")
}

func test_GetoptLongB1() {
  var argv = ["a.out", "-f"]
  var opts = GetoptLongOptions().noArgument("foo", 'f')
  var parsedOpts = Vector<GetoptLong.ErrorOrOption>()
  var nonOptions = Vector<String>()
  for r in GetoptLong(argv, opts, nonOptions) {
    parsedOpts.append(r)
  }

  assert(parsedOpts.length == 1, "B1_1")

  assert(!parsedOpts[0].isError, "B1_2")
  assert(parsedOpts[0].optionIndex.get() == 0, "B1_3")
  assert(parsedOpts[0].arg.isNone(), "B1_4")

  assert(nonOptions.isEmpty, "B1_5")
}

func test_GetoptLongB2() {
  var argv = ["a.out", "-f"]
  var opts = GetoptLongOptions().noArgument("foo")
                                .noArgument("", 'f')
  var parsedOpts = Vector<GetoptLong.ErrorOrOption>()
  var nonOptions = Vector<String>()
  for r in GetoptLong(argv, opts, nonOptions) {
    parsedOpts.append(r)
  }

  assert(parsedOpts.length == 1, "B2_1")

  assert(!parsedOpts[0].isError, "B2_2")
  assert(parsedOpts[0].optionIndex.get() == 1, "B2_3")
  assert(parsedOpts[0].arg.isNone(), "B2_4")

  assert(nonOptions.isEmpty, "B2_5")
}

func test_GetoptLongB3() {
  var argv = ["a.out", "-f", "-b", "--foo"]
  var opts = GetoptLongOptions().noArgument("foo")
                                .noArgument("", 'f')
                                .noArgument("bar", 'b')
  var parsedOpts = Vector<GetoptLong.ErrorOrOption>()
  var nonOptions = Vector<String>()
  for r in GetoptLong(argv, opts, nonOptions) {
    parsedOpts.append(r)
  }

  assert(parsedOpts.length == 3, "B3_1")

  assert(!parsedOpts[0].isError, "B3_2")
  assert(parsedOpts[0].optionIndex.get() == 1, "B3_3")
  assert(parsedOpts[0].arg.isNone(), "B3_4")

  assert(!parsedOpts[1].isError, "B3_5")
  assert(parsedOpts[1].optionIndex.get() == 2, "B3_6")
  assert(parsedOpts[1].arg.isNone(), "B3_7")

  assert(!parsedOpts[2].isError, "B3_8")
  assert(parsedOpts[2].optionIndex.get() == 0, "B3_9")
  assert(parsedOpts[2].arg.isNone(), "B3_10")

  assert(nonOptions.isEmpty, "B3_11")
}

func test_GetoptLongC0() {
  var argv = ["a.out", "--foo"]
  var opts = GetoptLongOptions().requiredArgument("foo")
  var parsedOpts = Vector<GetoptLong.ErrorOrOption>()
  var nonOptions = Vector<String>()
  for r in GetoptLong(argv, opts, nonOptions) {
    parsedOpts.append(r)
  }

  assert(parsedOpts.length == 1, "C0_1")

  assert(parsedOpts[0].missingArgument, "C0_2")
  assert(parsedOpts[0].optionIndex.isNone(), "C0_3")
  assert(parsedOpts[0].arg.isNone(), "C0_4")

  assert(nonOptions.isEmpty, "C0_5")
}

func test_GetoptLongC1() {
  var argv = ["a.out", "--foo", "meow"]
  var opts = GetoptLongOptions().noArgument("bar")
                                .requiredArgument("foo")
  var parsedOpts = Vector<GetoptLong.ErrorOrOption>()
  var nonOptions = Vector<String>()
  for r in GetoptLong(argv, opts, nonOptions) {
    parsedOpts.append(r)
  }

  assert(parsedOpts.length == 1, "C1_1")

  assert(!parsedOpts[0].isError, "C1_2")
  assert(parsedOpts[0].optionIndex.get() == 1, "C1_3")
  assert(parsedOpts[0].arg.get() == "meow", "C1_4")

  assert(nonOptions.isEmpty, "C1_5")
}

func test_GetoptLongD0() {
  var argv = ["a.out", "--bar"]
  var opts = GetoptLongOptions().noArgument("foo")
                                .optionalArgument("bar")
  var parsedOpts = Vector<GetoptLong.ErrorOrOption>()
  var nonOptions = Vector<String>()
  for r in GetoptLong(argv, opts, nonOptions) {
    parsedOpts.append(r)
  }

  assert(parsedOpts.length == 1, "D0_1")

  assert(!parsedOpts[0].isError, "D0_2")
  assert(parsedOpts[0].optionIndex.get() == 1, "D0_3")
  assert(parsedOpts[0].arg.isNone(), "D0_4")

  assert(nonOptions.isEmpty, "D0_5")
}

func test_GetoptLongD1() {
  var argv = ["a.out", "--bar", "meow"]
  var opts = GetoptLongOptions().noArgument("foo")
                                .optionalArgument("bar")
  var parsedOpts = Vector<GetoptLong.ErrorOrOption>()
  var nonOptions = Vector<String>()
  for r in GetoptLong(argv, opts, nonOptions) {
    parsedOpts.append(r)
  }

  assert(parsedOpts.length == 1, "D1_1")

  assert(!parsedOpts[0].isError, "D1_2")
  assert(parsedOpts[0].optionIndex.get() == 1, "D1_3")

  // Yes, arg.isNone() is expected.  --bar=meow is the correct syntax.
  assert(parsedOpts[0].arg.isNone(), "D1_4")

  assert(nonOptions.length == 1, "D1_5")
  assert(nonOptions[0] == "meow", "D1_6")
}

func test_GetoptLongD2() {
  var argv = ["a.out", "--bar=meow"]
  var opts = GetoptLongOptions().noArgument("foo")
                                .optionalArgument("bar")
  var parsedOpts = Vector<GetoptLong.ErrorOrOption>()
  var nonOptions = Vector<String>()
  for r in GetoptLong(argv, opts, nonOptions) {
    parsedOpts.append(r)
  }

  assert(parsedOpts.length == 1, "D2_1")

  assert(!parsedOpts[0].isError, "D2_2")
  assert(parsedOpts[0].optionIndex.get() == 1, "D2_3")
  assert(parsedOpts[0].arg.get() == "meow", "D2_4")

  assert(nonOptions.isEmpty, "D2_5")
}

func test_GetoptLongE0() {
  var argv = ["a.out", "--bar", "meow", "--foo", "zzz"]
  var opts = GetoptLongOptions().noArgument("foo")
                                .optionalArgument("bar")
  var parsedOpts = Vector<GetoptLong.ErrorOrOption>()
  var nonOptions = Vector<String>()
  for r in GetoptLong(argv, opts, nonOptions) {
    parsedOpts.append(r)
  }

  assert(parsedOpts.length == 2, "E0_1")

  assert(!parsedOpts[0].isError, "E0_2")
  assert(parsedOpts[0].optionIndex.get() == 1, "E0_3")
  assert(parsedOpts[0].arg.isNone(), "E0_4")

  assert(!parsedOpts[1].isError, "E0_5")
  assert(parsedOpts[1].optionIndex.get() == 0, "E0_6")
  assert(parsedOpts[1].arg.isNone(), "E0_7")

  assert(nonOptions.length == 2, "E0_8")
  assert(nonOptions[0] == "meow", "E0_9")
  assert(nonOptions[1] == "zzz", "E0_10")
}

func test_GetoptLongE1() {
  var argv = ["a.out", "--bar", "--", "--foo", "meow"]
  var opts = GetoptLongOptions().noArgument("foo")
                                .optionalArgument("bar")
  var parsedOpts = Vector<GetoptLong.ErrorOrOption>()
  var nonOptions = Vector<String>()
  for r in GetoptLong(argv, opts, nonOptions) {
    parsedOpts.append(r)
  }

  assert(parsedOpts.length == 1, "E1_1")

  assert(!parsedOpts[0].isError, "E1_2")
  assert(parsedOpts[0].optionIndex.get() == 1, "E1_3")
  assert(parsedOpts[0].arg.isNone(), "E1_4")

  assert(nonOptions.length == 2, "E1_5")
  assert(nonOptions[0] == "--foo", "E2_6")
  assert(nonOptions[1] == "meow", "E2_6")
}

func test_GetoptLong() {
  test_GetoptLongA0()
  test_GetoptLongA1()
  test_GetoptLongA2()
  test_GetoptLongA3()
  test_GetoptLongA4()
  test_GetoptLongA5()
  test_GetoptLongA6()

  test_GetoptLongB0()
  test_GetoptLongB1()
  test_GetoptLongB2()
  test_GetoptLongB3()

  test_GetoptLongC0()
  test_GetoptLongC1()

  test_GetoptLongD0()
  test_GetoptLongD1()
  test_GetoptLongD2()

  test_GetoptLongE0()
  test_GetoptLongE1()
}

test_GetoptLong()
println("ALL OK") // CHECK: ALL OK

