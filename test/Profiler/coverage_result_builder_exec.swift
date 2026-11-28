// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -swift-version 6 -profile-generate -profile-coverage-mapping -o %t/main
// RUN: %target-codesign %t/main
// RUN: env %env-LLVM_PROFILE_FILE=%t/mixed.profraw %target-run %t/main 145 1 5
// RUN: %llvm-profdata merge %t/mixed.profraw -o %t/mixed.profdata
// RUN: %llvm-cov export %t/main -instr-profile=%t/mixed.profdata | %FileCheck %s --check-prefix=MIXED
// RUN: env %env-LLVM_PROFILE_FILE=%t/failure.profraw %target-run %t/main 2 1 0
// RUN: %llvm-profdata merge %t/failure.profraw -o %t/failure.profdata
// RUN: %llvm-cov export %t/main -instr-profile=%t/failure.profdata | %FileCheck %s --check-prefix=FAILURE
// RUN: env %env-LLVM_PROFILE_FILE=%t/ready.profraw %target-run %t/main 145 0 0
// RUN: %llvm-profdata merge %t/ready.profraw -o %t/ready.profdata
// RUN: %llvm-cov export %t/main -instr-profile=%t/ready.profdata | %FileCheck %s --check-prefix=READY
// REQUIRES: profile_runtime
// REQUIRES: executable_test

@resultBuilder
enum Builder {
  static func buildBlock(_ parts: Int...) -> Int { parts.reduce(0, +) }
  static func buildOptional(_ value: Int?) -> Int { value ?? 0 }
  static func buildEither(first value: Int) -> Int { value }
  static func buildEither(second value: Int) -> Int { value }
}

func build(@Builder _ content: () -> Int) -> Int { content() }

func render(problem: String?, ready: Bool) -> Int {
  build {
    if let problem { problem.count }
    if ready {
      1
    } else if problem == nil {
      2
    }
  }
}

// MIXED:   [[[@LINE-6]],15,6,true,true,false]
// MIXED:   [[[@LINE-7]],30,5,true,true,false]
// FAILURE: [[[@LINE-8]],15,1,true,true,false]
// FAILURE: [[[@LINE-9]],30,0,true,true,false]
// READY:   [[[@LINE-10]],15,0,true,true,false]
// READY:   [[[@LINE-11]],30,0,true,true,false]

for _ in 0..<Int(CommandLine.arguments[1])! {
  _ = render(problem: nil, ready: true)
}
for _ in 0..<Int(CommandLine.arguments[2])! {
  _ = render(problem: "Expected failure", ready: false)
}
for _ in 0..<Int(CommandLine.arguments[3])! {
  _ = render(problem: nil, ready: false)
}
