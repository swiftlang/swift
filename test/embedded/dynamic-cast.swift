// RUN: %target-swift-emit-ir %s -enable-experimental-feature Embedded -parse-as-library -module-name main | %FileCheck -check-prefix=IR %s

// RUN: %target-run-simple-swift( -enable-experimental-feature Embedded -parse-as-library -wmo %target-embedded-posix-shim -g) | %FileCheck %s --check-prefix=OUTPUT

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

protocol P { }
protocol CP: AnyObject { }

struct S: P, Error { }

struct BigStruct: P, Error {
  let sup = Super()
  let x: Int = 1
  let y: Int = 2
  let z: Int = 3
}

var globalCounter: Int = 0

class Super: P, CP, Error {
  init() {
    globalCounter += 1
  }

  deinit {
    precondition(globalCounter > 0)
    globalCounter -= 1
  }
}
class Sub: Super { }
class SubSub: Sub { }

enum EnumError: Error, P {
case failed
case didNotTry
}

@c
func arc4random_buf(_ buf: UnsafeMutableRawPointer, _ nbytes: Int) { }

@inline(never)
func doTryCastFromAny<T>(_ value: Any, to: T.Type) -> T? {
  value as? T
}

// IR-LABEL: define linkonce_odr hidden swiftcc {{(i32|i64)}} @"$e4main16doTryCastFromAny_2toxSgyp_xmtlFAA03SubH0C_Tt1g5"
// IR: call zeroext i1 @swift_dynamicCast({{.*}}ptr @"$eypMf"

// IR-LABEL: testAnyExistentials
func testAnyExistentials() {
  print("Testing Any existentials...")

  // Structs and enums
  precondition(doTryCastFromAny(S(), to: S.self) != nil)
  precondition(doTryCastFromAny(EnumError.failed, to: EnumError.self) != nil)
  precondition(doTryCastFromAny(S(), to: EnumError.self) == nil)
  precondition(doTryCastFromAny(EnumError.failed, to: S.self) == nil)
  precondition(doTryCastFromAny(BigStruct(), to: BigStruct.self) != nil)
  precondition(doTryCastFromAny(BigStruct(), to: EnumError.self) == nil)

  // Classes.
  let sup = Super()
  precondition(doTryCastFromAny(sup, to: Super.self) === sup)
  precondition(doTryCastFromAny(sup, to: Sub.self) == nil)
  precondition(doTryCastFromAny(sup, to: SubSub.self) == nil)
  precondition(doTryCastFromAny(sup, to: EnumError.self) == nil)
  precondition(doTryCastFromAny(Super(), to: Super.self) != nil)
  precondition(doTryCastFromAny(Super(), to: Sub.self) == nil)
  precondition(doTryCastFromAny(Super(), to: SubSub.self) == nil)
  precondition(doTryCastFromAny(Super(), to: EnumError.self) == nil)
  
  let sub = Sub()
  precondition(doTryCastFromAny(sub, to: Super.self) === sub)
  precondition(doTryCastFromAny(sub, to: Sub.self) === sub)
  precondition(doTryCastFromAny(sub, to: SubSub.self) == nil)
  precondition(doTryCastFromAny(Sub(), to: Super.self) != nil)
  precondition(doTryCastFromAny(Sub(), to: Sub.self) != nil)
  precondition(doTryCastFromAny(Sub(), to: SubSub.self) == nil)

  let subsub = SubSub()
  precondition(doTryCastFromAny(subsub, to: Super.self) === subsub)
  precondition(doTryCastFromAny(subsub, to: Sub.self) === subsub)
  precondition(doTryCastFromAny(subsub, to: SubSub.self) === subsub)
}

@inline(never)
func doTryCastFromP<T>(_ value: any P, to: T.Type) -> T? {
  value as? T
}

// IR: testOpaqueExistentials
func testOpaqueExistentials() {
  print("Testing opaque existentials...")

  // Structs and enums
  precondition(doTryCastFromP(S(), to: S.self) != nil)
  precondition(doTryCastFromP(EnumError.failed, to: EnumError.self) != nil)
  precondition(doTryCastFromP(S(), to: EnumError.self) == nil)
  precondition(doTryCastFromP(EnumError.failed, to: S.self) == nil)
  precondition(doTryCastFromP(BigStruct(), to: BigStruct.self) != nil)
  precondition(doTryCastFromP(BigStruct(), to: EnumError.self) == nil)

  // Classes.
  let sup = Super()
  precondition(doTryCastFromP(sup, to: Super.self) === sup)
  precondition(doTryCastFromP(sup, to: Sub.self) == nil)
  precondition(doTryCastFromP(sup, to: SubSub.self) == nil)
  precondition(doTryCastFromP(sup, to: EnumError.self) == nil)
  precondition(doTryCastFromP(Super(), to: Super.self) != nil)
  precondition(doTryCastFromP(Super(), to: Sub.self) == nil)
  precondition(doTryCastFromP(Super(), to: SubSub.self) == nil)
  precondition(doTryCastFromP(Super(), to: EnumError.self) == nil)
  
  let sub = Sub()
  precondition(doTryCastFromP(sub, to: Super.self) === sub)
  precondition(doTryCastFromP(sub, to: Sub.self) === sub)
  precondition(doTryCastFromP(sub, to: SubSub.self) == nil)
  precondition(doTryCastFromP(Sub(), to: Super.self) != nil)
  precondition(doTryCastFromP(Sub(), to: Sub.self) != nil)
  precondition(doTryCastFromP(Sub(), to: SubSub.self) == nil)

  let subsub = SubSub()
  precondition(doTryCastFromP(subsub, to: Super.self) === subsub)
  precondition(doTryCastFromP(subsub, to: Sub.self) === subsub)
  precondition(doTryCastFromP(subsub, to: SubSub.self) === subsub)
}

@inline(never)
func doTryCastFromAnyObject<T>(_ value: AnyObject, to: T.Type) -> T? {
  value as? T
}


// IR-LABEL: testAnyObjectExistentials
func testAnyObjectExistentials() {
  print("Testing AnyObject existentials...")

  // Classes.
  let sup = Super()
  precondition(doTryCastFromAnyObject(sup, to: Super.self) === sup)
  precondition(doTryCastFromAnyObject(sup, to: Sub.self) == nil)
  precondition(doTryCastFromAnyObject(sup, to: SubSub.self) == nil)
  precondition(doTryCastFromAnyObject(sup, to: EnumError.self) == nil)
  precondition(doTryCastFromAnyObject(Super(), to: Super.self) != nil)
  precondition(doTryCastFromAnyObject(Super(), to: Sub.self) == nil)
  precondition(doTryCastFromAnyObject(Super(), to: SubSub.self) == nil)
  precondition(doTryCastFromAnyObject(Super(), to: EnumError.self) == nil)
  
  let sub = Sub()
  precondition(doTryCastFromAnyObject(sub, to: Super.self) === sub)
  precondition(doTryCastFromAnyObject(sub, to: Sub.self) === sub)
  precondition(doTryCastFromAnyObject(sub, to: SubSub.self) == nil)
  precondition(doTryCastFromAnyObject(Sub(), to: Super.self) != nil)
  precondition(doTryCastFromAnyObject(Sub(), to: Sub.self) != nil)
  precondition(doTryCastFromAnyObject(Sub(), to: SubSub.self) == nil)

  let subsub = SubSub()
  precondition(doTryCastFromAnyObject(subsub, to: Super.self) === subsub)
  precondition(doTryCastFromAnyObject(subsub, to: Sub.self) === subsub)
  precondition(doTryCastFromAnyObject(subsub, to: SubSub.self) === subsub)
}

@inline(never)
func doTryCastFromCP<T>(_ value: any CP, to: T.Type) -> T? {
  value as? T
}

// IR: testClassBoundExistentials
func testClassBoundExistentials() {
  print("Testing class-bound existentials...")

  // Classes.
  let sup = Super()
  precondition(doTryCastFromCP(sup, to: Super.self) === sup)
  precondition(doTryCastFromCP(sup, to: Sub.self) == nil)
  precondition(doTryCastFromCP(sup, to: SubSub.self) == nil)
  precondition(doTryCastFromCP(sup, to: EnumError.self) == nil)
  precondition(doTryCastFromCP(Super(), to: Super.self) != nil)
  precondition(doTryCastFromCP(Super(), to: Sub.self) == nil)
  precondition(doTryCastFromCP(Super(), to: SubSub.self) == nil)
  precondition(doTryCastFromCP(Super(), to: EnumError.self) == nil)
  
  let sub = Sub()
  precondition(doTryCastFromCP(sub, to: Super.self) === sub)
  precondition(doTryCastFromCP(sub, to: Sub.self) === sub)
  precondition(doTryCastFromCP(sub, to: SubSub.self) == nil)
  precondition(doTryCastFromCP(Sub(), to: Super.self) != nil)
  precondition(doTryCastFromCP(Sub(), to: Sub.self) != nil)
  precondition(doTryCastFromCP(Sub(), to: SubSub.self) == nil)

  let subsub = SubSub()
  precondition(doTryCastFromCP(subsub, to: Super.self) === subsub)
  precondition(doTryCastFromCP(subsub, to: Sub.self) === subsub)
  precondition(doTryCastFromCP(subsub, to: SubSub.self) === subsub)
}

@inline(never)
func doTryCastFromError<T>(_ value: any Error, to: T.Type) -> T? {
  value as? T
}

// IR: testErrorExistentials
func testErrorExistentials() {
  print("Testing error existentials...")

  // Structs and enums
  precondition(doTryCastFromError(S(), to: S.self) != nil)
  precondition(doTryCastFromError(EnumError.failed, to: EnumError.self) != nil)
  precondition(doTryCastFromError(S(), to: EnumError.self) == nil)
  precondition(doTryCastFromError(EnumError.failed, to: S.self) == nil)
  precondition(doTryCastFromError(BigStruct(), to: BigStruct.self) != nil)
  precondition(doTryCastFromError(BigStruct(), to: EnumError.self) == nil)

  // Classes.
  let sup = Super()
  precondition(doTryCastFromError(sup, to: Super.self) === sup)
  precondition(doTryCastFromError(sup, to: Sub.self) == nil)
  precondition(doTryCastFromError(sup, to: SubSub.self) == nil)
  precondition(doTryCastFromError(sup, to: EnumError.self) == nil)
  precondition(doTryCastFromError(Super(), to: Super.self) != nil)
  precondition(doTryCastFromError(Super(), to: Sub.self) == nil)
  precondition(doTryCastFromError(Super(), to: SubSub.self) == nil)
  precondition(doTryCastFromError(Super(), to: EnumError.self) == nil)
  
  let sub = Sub()
  precondition(doTryCastFromError(sub, to: Super.self) === sub)
  precondition(doTryCastFromError(sub, to: Sub.self) === sub)
  precondition(doTryCastFromError(sub, to: SubSub.self) == nil)
  precondition(doTryCastFromError(Sub(), to: Super.self) != nil)
  precondition(doTryCastFromError(Sub(), to: Sub.self) != nil)
  precondition(doTryCastFromError(Sub(), to: SubSub.self) == nil)

  let subsub = SubSub()
  precondition(doTryCastFromError(subsub, to: Super.self) === subsub)
  precondition(doTryCastFromError(subsub, to: Sub.self) === subsub)
  precondition(doTryCastFromError(subsub, to: SubSub.self) === subsub)
}

@main
struct Main {
  static func main() {
    // OUTPUT: Testing Any existentials...
    testAnyExistentials()

    // OUTPUT: Testing opaque existentials...
    testOpaqueExistentials()

    // OUTPUT: Testing AnyObject existentials...
    testAnyObjectExistentials()

    // OUTPUT: Testing class-bound existentials...
    testClassBoundExistentials()

    // OUTPUT: Testing error existentials...
    testErrorExistentials()

    // Make sure all of the instances we created also got destroyed.
    precondition(globalCounter == 0)

    // OUTPUT: Done
    print("Done")
  }
}
