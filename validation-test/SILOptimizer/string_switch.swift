// RUN: %target-build-swift -O %s -module-name=test -Xllvm -sil-disable-pass=FunctionSignatureOpts -o %t.out
// RUN: %target-build-swift -O %s -module-name=test -Xllvm -sil-disable-pass=FunctionSignatureOpts -emit-sil | %FileCheck %s
// RUN: %target-codesign %t.out
// RUN: %target-run %t.out
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib
// REQUIRES: stress_test
// REQUIRES: executable_test
// UNSUPPORTED: threading_none


import StdlibUnittest

// CHECK-LABEL: sil hidden @$s4test9SmallEnumO8rawValueACSgSS_tcfC :
// CHECK-DAG:     global_value @{{.*}}SmallEnum{{.*}}rawValue{{.*}}
// CHECK-DAG:     // function_ref _findStringSwitchCase(cases:string:)
// CHECK:       } // end sil function '$s4test9SmallEnumO8rawValueACSgSS_tcfC'

enum SmallEnum : String {
  case c1 = "Swift"
  case c2 = "is"
  case c3 = "a"
  case c4 = "general-purpose"
  case c5 = "programming language"
}

// CHECK-LABEL: sil hidden @$s4test9LargeEnumO8rawValueACSgSS_tcfC :
// CHECK-DAG:     global_value @{{.*}}LargeEnum{{.*}}rawValue{{.*}}
// CHECK-DAG:     // function_ref _findStringSwitchCaseWithCache(cases:string:cache:)
// CHECK:       } // end sil function '$s4test9LargeEnumO8rawValueACSgSS_tcfC'

enum LargeEnum : String {
  case c1 = "Swift"
  case c2 = "is"
  case c3 = "a"
  case c4 = "general-purpose"
  case c5 = "programming language"
  case c7 = "built"
  case c8 = "using"
  case c10 = "modern"
  case c11 = "approach"
  case c12 = "to"
  case c13 = "safety,"
  case c14 = "performance,"
  case c15 = "and"
  case c16 = "software"
  case c17 = "design"
  case c18 = "patterns."
  case c19 = ""
  case c20 = "The"
  case c21 = "goal"
  case c22 = "of"
  case c23 = "the"
  case c25 = "project"
  case c28 = "create"
  case c30 = "best"
  case c31 = "available"
  case c33 = "for"
  case c34 = "uses"
  case c35 = "ranging"
  case c36 = "from"
  case c37 = "systems"
  case c40 = "mobile"
  case c42 = "desktop"
  case c43 = "apps,"
  case c44 = "scaling"
  case c45 = "up"
  case c47 = "cloud"
  case c48 = "services."
}

let SmallInputs : [(String, SmallEnum)] = [
  ("Swift", SmallEnum.c1),
  ("is", SmallEnum.c2),
  ("a", SmallEnum.c3),
  ("general-purpose", SmallEnum.c4),
  ("programming language", SmallEnum.c5),
]

let LargeInputs : [(String, LargeEnum)] = [
  ("Swift", LargeEnum.c1),
  ("is", LargeEnum.c2),
  ("a", LargeEnum.c3),
  ("general-purpose", LargeEnum.c4),
  ("programming language", LargeEnum.c5),
  ("built", LargeEnum.c7),
  ("using", LargeEnum.c8),
  ("modern", LargeEnum.c10),
  ("approach", LargeEnum.c11),
  ("to", LargeEnum.c12),
  ("safety,", LargeEnum.c13),
  ("performance,", LargeEnum.c14),
  ("and", LargeEnum.c15),
  ("software", LargeEnum.c16),
  ("design", LargeEnum.c17),
  ("patterns.", LargeEnum.c18),
  ("", LargeEnum.c19),
  ("The", LargeEnum.c20),
  ("goal", LargeEnum.c21),
  ("of", LargeEnum.c22),
  ("the", LargeEnum.c23),
  ("project", LargeEnum.c25),
  ("create", LargeEnum.c28),
  ("best", LargeEnum.c30),
  ("available", LargeEnum.c31),
  ("for", LargeEnum.c33),
  ("uses", LargeEnum.c34),
  ("ranging", LargeEnum.c35),
  ("from", LargeEnum.c36),
  ("systems", LargeEnum.c37),
  ("mobile", LargeEnum.c40),
  ("desktop", LargeEnum.c42),
  ("apps,", LargeEnum.c43),
  ("scaling", LargeEnum.c44),
  ("up", LargeEnum.c45),
  ("cloud", LargeEnum.c47),
  ("services.", LargeEnum.c48)
]

var StringSwitchTestSuite = TestSuite("StringSwitch")

StringSwitchTestSuite.test("small_enum_init") {
  runRaceTest(trials: 10, timeoutInSeconds: 0) {
    for i in SmallInputs {
      expectEqual(SmallEnum(rawValue: i.0), i.1)
    }
  }
}

StringSwitchTestSuite.test("large_enum_init") {
  runRaceTest(trials: 10, timeoutInSeconds: 0) {
    for i in LargeInputs {
      expectEqual(LargeEnum(rawValue: i.0), i.1)
    }
  }
}

runAllTests()

