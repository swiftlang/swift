// RUN: %target-build-swift -O %s -module-name=test -Xllvm -sil-disable-pass=FunctionSignatureOpts -o %t.out
// RUN: %target-build-swift -O %s -module-name=test -Xllvm -sil-disable-pass=FunctionSignatureOpts -emit-sil | %FileCheck %s
// RUN: %target-run %t.out
// UNSUPPORTED: nonatomic_rc

import StdlibUnittest

// CHECK-LABEL: // SmallEnum.init(rawValue:)
// CHECK-NEXT:  sil {{.*}} @{{.*}}SmallEnum{{.*}}rawValue{{.*}} :
// CHECK-DAG:     global_value @{{.*}}SmallEnum{{.*}}rawValue{{.*}}
// CHECK-DAG:     // function_ref _findStringSwitchCase(cases:string:)
// CHECK:         return

enum SmallEnum : String {
  case c1 = "Swift"
  case c2 = "is"
  case c3 = "a"
  case c4 = "general-purpose"
  case c5 = "programming language"
}

let SmallInputs : [(String, SmallEnum)] = [
  ("Swift", SmallEnum.c1),
  ("is", SmallEnum.c2),
  ("a", SmallEnum.c3),
  ("general-purpose", SmallEnum.c4),
  ("programming language", SmallEnum.c5),
]

var StringSwitchTestSuite = TestSuite("StringSwitch")

StringSwitchTestSuite.test("small_enum_init") {
  runRaceTest(trials: 10, timeoutInSeconds: 0) {
    for i in SmallInputs {
      expectEqual(SmallEnum(rawValue: i.0), i.1)
    }
  }
}

runAllTests()

