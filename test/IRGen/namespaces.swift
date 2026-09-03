// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-ir -g -parse-as-library \
// RUN:   -module-name main -enable-experimental-feature Namespaces %s \
// RUN:   > %t/legacy.ll
// RUN: %target-swift-frontend -emit-ir -g -parse-as-library \
// RUN:   -module-name main -enable-experimental-feature Namespaces \
// RUN:   -enable-experimental-feature ParserASTGen %s > %t/astgen.ll
// RUN: %diff -u %t/legacy.ll %t/astgen.ll
// RUN: %FileCheck %s --check-prefix=IR < %t/legacy.ll

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_Namespaces
// UNSUPPORTED: asan

namespace BuildProof {
  static func answer() -> Int { 42 }
}

// IR-NOT: BuildProofyXZMa
// IR-NOT: BuildProofyXZMn
// IR-NOT: BuildProofyXZN
// IR-NOT: BuildProofyXZMXX
// IR-LABEL: define hidden swiftcc i64 @"$s4main10BuildProofyXZ6answerSiyF"()
// IR: ret i64 42
// IR-LABEL: define hidden swiftcc i64 @"$s4main10callAnswerSiyF"()
// IR: call swiftcc i64 @"$s4main10BuildProofyXZ6answerSiyF"()
// IR-NOT: BuildProofyXZMa
// IR-NOT: BuildProofyXZMn
// IR-NOT: BuildProofyXZN
// IR-NOT: BuildProofyXZMXX
func callAnswer() -> Int {
  BuildProof.answer()
}
