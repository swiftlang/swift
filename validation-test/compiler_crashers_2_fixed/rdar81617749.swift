// RUN: %empty-directory(%t)
// RUN: %target-clang %S/Inputs/rdar81617749.m -I %S/Inputs -c -o %t/rdar81617749.o
// RUN: %target-build-swift -Xfrontend -disable-availability-checking -import-objc-header %S/Inputs/rdar81617749.h -Xlinker %t/rdar81617749.o -parse-as-library %s -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

// Enable with rdar://81617749
// UNSUPPORTED: CPU=i386 && OS=watchos

// rdar://82123254
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

func run(on object: PFXObject) async throws {
  // CHECK: performSingleFlaggy1
  print(try await object.performSingleFlaggy1()?())
  // CHECK: performSingleFlaggy2
  print(try await object.performSingleFlaggy2()?())
  // CHECK: performSingleErrory1
  print(try await object.performSingleErrory1()())
  // CHECK: performSingleErrory2
  print(try await object.performSingleErrory2()())

  // CHECK: performSingleBothy12
  print(try await object.performSingleBothy12()())
  // CHECK: performSingleBothy13
  print(try await object.performSingleBothy13()())

  // CHECK: performSingleBothy21
  print(try await object.performSingleBothy21()())
  
  // CHECK: performSingleBothy23
  print(try await object.performSingleBothy23()())
  // CHECK: performSingleBothy31
  print(try await object.performSingleBothy31()())
  // CHECK: performSingleBothy32
  print(try await object.performSingleBothy32()())

  // CHECK: performDoubleFlaggy1, part 1
  // CHECK: performDoubleFlaggy1, part 2
  let rFlaggy1 = try await object.performDoubleFlaggy1()
  rFlaggy1.0?()
  rFlaggy1.1?()
  // CHECK: performDoubleFlaggy2, part 1
  // CHECK: performDoubleFlaggy2, part 2
  let rFlaggy2 = try await object.performDoubleFlaggy2()
  rFlaggy2.0?()
  rFlaggy2.1?()
  // CHECK: performDoubleFlaggy3, part 1
  // CHECK: performDoubleFlaggy3, part 2
  let rFlaggy3 = try await object.performDoubleFlaggy3()
  rFlaggy3.0?()
  rFlaggy3.1?()

  // CHECK: performDoubleErrory1, part 1
  // CHECK: performDoubleErrory1, part 2
  let rErrory1 = try await object.performDoubleErrory1()
  rErrory1.0()
  rErrory1.1()
  // CHECK: performDoubleErrory2, part 1
  // CHECK: performDoubleErrory2, part 2
  let rErrory2 = try await object.performDoubleErrory2()
  rErrory2.0()
  rErrory2.1()
  // CHECK: performDoubleErrory3, part 1
  // CHECK: performDoubleErrory3, part 2
  let rErrory3 = try await object.performDoubleErrory3()
  rErrory3.0()
  rErrory3.1()

  // CHECK: performDoubleBothy12, part 1
  // CHECK: performDoubleBothy12, part 2
  let rBothy12 = try await object.performDoubleBothy12()
  rBothy12.0()
  rBothy12.1()
  // CHECK: performDoubleBothy13, part 1
  // CHECK: performDoubleBothy13, part 2
  let rBothy13 = try await object.performDoubleBothy13()
  rBothy13.0()
  rBothy13.1()
  // CHECK: performDoubleBothy14, part 1
  // CHECK: performDoubleBothy14, part 2
  let rBothy14 = try await object.performDoubleBothy14()
  rBothy14.0()
  rBothy14.1()

  // CHECK: performDoubleBothy21, part 1
  // CHECK: performDoubleBothy21, part 2
  let rBothy21 = try await object.performDoubleBothy21()
  rBothy21.0()
  rBothy21.1()
  // CHECK: performDoubleBothy23, part 1
  // CHECK: performDoubleBothy23, part 2
  let rBothy23 = try await object.performDoubleBothy23()
  rBothy23.0()
  rBothy23.1()
  // CHECK: performDoubleBothy24, part 1
  // CHECK: performDoubleBothy24, part 2
  let rBothy24 = try await object.performDoubleBothy24()
  rBothy24.0()
  rBothy24.1()

  // CHECK: performDoubleBothy31, part 1
  // CHECK: performDoubleBothy31, part 2
  let rBothy31 = try await object.performDoubleBothy31()
  rBothy31.0()
  rBothy31.1()
  // CHECK: performDoubleBothy32, part 1
  // CHECK: performDoubleBothy32, part 2
  let rBothy32 = try await object.performDoubleBothy32()
  rBothy32.0()
  rBothy32.1()
  // CHECK: performDoubleBothy34, part 1
  // CHECK: performDoubleBothy34, part 2
  let rBothy34 = try await object.performDoubleBothy34()
  rBothy34.0()
  rBothy34.1()

  // CHECK: performDoubleBothy41, part 1
  // CHECK: performDoubleBothy41, part 2
  let rBothy41 = try await object.performDoubleBothy41()
  rBothy41.0()
  rBothy41.1()
  // CHECK: performDoubleBothy42, part 1
  // CHECK: performDoubleBothy42, part 2
  let rBothy42 = try await object.performDoubleBothy42()
  rBothy42.0()
  rBothy42.1()
  // CHECK: performDoubleBothy43, part 1
  // CHECK: performDoubleBothy43, part 2
  let rBothy43 = try await object.performDoubleBothy43()
  rBothy43.0()
  rBothy43.1()
}

@main struct Main {
  static func main() async throws {
    let object = PFXObject()
    try await run(on: object)
  }
}
