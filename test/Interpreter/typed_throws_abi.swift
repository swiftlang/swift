// RUN: %target-build-swift-dylib(%t/%target-library-name(TypedThrowsABI)) -enable-library-evolution %S/Inputs/typed_throws_abi_impl.swift -emit-module -emit-module-path %t/TypedThrowsABI.swiftmodule -module-name TypedThrowsABI

// RUN: %target-build-swift -parse-as-library -Xfrontend -disable-availability-checking %s -lTypedThrowsABI -I %t -L %t -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main %t/%target-library-name(TypedThrowsABI)
// RUN: %target-run %t/main %t/%target-library-name(TypedThrowsABI) | %FileCheck %s

// REQUIRES: executable_test

import TypedThrowsABI

func invoke<E: Error, each T>(_ f: () async throws(E) -> (repeat each T)) async {
    do {
        let res = try await f()
        print("Success: \(res)")
    } catch {
        print("Error: \(error)")
    }
}

func checkSync() async {
    let impl = Impl()

    // CHECK: Success: ()
    await invoke { try impl.f0(true) }
    // CHECK: Error: Empty()
    await invoke { try impl.f0(false) }

    // CHECK: Success: 1
    await invoke { try impl.f1(true) }
    // CHECK: Error: Empty()
    await invoke { try impl.f1(false) }

    // CHECK: Success: (1, 2)
    await invoke { try impl.f2(true) }
    // CHECK: Error: Empty()
    await invoke { try impl.f2(false) }

    // CHECK: Success: (1, 2, 3)
    await invoke { try impl.f3(true) }
    // CHECK: Error: Empty()
    await invoke { try impl.f3(false) }

    // CHECK: Success: (1, 2, 3, 4)
    await invoke { try impl.f4(true) }
    // CHECK: Error: Empty()
    await invoke { try impl.f4(false) }

    // CHECK: Success: (1, 2, 3, 4, 5)
    await invoke { try impl.f5(true) }
    // CHECK: Error: Empty()
    await invoke { try impl.f5(false) }

    // CHECK: Success: ()
    await invoke { try impl.g0(true) }
    // CHECK: Error: OneWord(x: 0)
    await invoke { try impl.g0(false) }

    // CHECK: Success: 1
    await invoke { try impl.g1(true) }
    // CHECK: Error: OneWord(x: 0)
    await invoke { try impl.g1(false) }

    // CHECK: Success: (1, 2)
    await invoke { try impl.g2(true) }
    // CHECK: Error: OneWord(x: 0)
    await invoke { try impl.g2(false) }

    // CHECK: Success: (1, 2, 3)
    await invoke { try impl.g3(true) }
    // CHECK: Error: OneWord(x: 0)
    await invoke { try impl.g3(false) }

    // CHECK: Success: (1, 2, 3, 4)
    await invoke { try impl.g4(true) }
    // CHECK: Error: OneWord(x: 0)
    await invoke { try impl.g4(false) }

    // CHECK: Success: (1, 2, 3, 4, 5)
    await invoke { try impl.g5(true) }
    // CHECK: Error: OneWord(x: 0)
    await invoke { try impl.g5(false) }

    // CHECK: Success: ()
    await invoke { try impl.h0(true) }
    // CHECK: Error: TwoWords(x: 0, y: 0)
    await invoke { try impl.h0(false) }

    // CHECK: Success: 1
    await invoke { try impl.h1(true) }
    // CHECK: Error: TwoWords(x: 0, y: 0)
    await invoke { try impl.h1(false) }

    // CHECK: Success: (1, 2)
    await invoke { try impl.h2(true) }
    // CHECK: Error: TwoWords(x: 0, y: 0)
    await invoke { try impl.h2(false) }

    // CHECK: Success: (1, 2, 3)
    await invoke { try impl.h3(true) }
    // CHECK: Error: TwoWords(x: 0, y: 0)
    await invoke { try impl.h3(false) }

    // CHECK: Success: (1, 2, 3, 4)
    await invoke { try impl.h4(true) }
    // CHECK: Error: TwoWords(x: 0, y: 0)
    await invoke { try impl.h4(false) }

    // CHECK: Success: (1, 2, 3, 4, 5)
    await invoke { try impl.h5(true) }
    // CHECK: Error: TwoWords(x: 0, y: 0)
    await invoke { try impl.h5(false) }

    // CHECK: Success: ()
    await invoke { try impl.i0(true) }
    // CHECK: Error: ThreeWords(x: 0, y: 0, z: 0)
    await invoke { try impl.i0(false) }

    // CHECK: Success: 1
    await invoke { try impl.i1(true) }
    // CHECK: Error: ThreeWords(x: 0, y: 0, z: 0)
    await invoke { try impl.i1(false) }

    // CHECK: Success: (1, 2)
    await invoke { try impl.i2(true) }
    // CHECK: Error: ThreeWords(x: 0, y: 0, z: 0)
    await invoke { try impl.i2(false) }

    // CHECK: Success: (1, 2, 3)
    await invoke { try impl.i3(true) }
    // CHECK: Error: ThreeWords(x: 0, y: 0, z: 0)
    await invoke { try impl.i3(false) }

    // CHECK: Success: (1, 2, 3, 4)
    await invoke { try impl.i4(true) }
    // CHECK: Error: ThreeWords(x: 0, y: 0, z: 0)
    await invoke { try impl.i4(false) }

    // CHECK: Success: (1, 2, 3, 4, 5)
    await invoke { try impl.i5(true) }
    // CHECK: Error: ThreeWords(x: 0, y: 0, z: 0)
    await invoke { try impl.i5(false) }

    // CHECK: Success: (1.0, 2.0)
    await invoke { try impl.nonMatching_f0(true) }
    // CHECK: Error: OneWord(x: 0)
    await invoke { try impl.nonMatching_f0(false) }

    // CHECK: Success: (1.0, true, 2.0)
    await invoke { try impl.nonMatching_f1(true) }
    // CHECK: Error: OneWord(x: 0)
    await invoke { try impl.nonMatching_f1(false) }
}

func checkAsync() async {
    let impl = ImplAsync()
    // CHECK: Success: ()
    await invoke { try await impl.f0(true) }
    // CHECK: Error: Empty()
    await invoke { try await impl.f0(false) }

    // CHECK: Success: 1
    await invoke { try await impl.f1(true) }
    // CHECK: Error: Empty()
    await invoke { try await impl.f1(false) }

    // CHECK: Success: (1, 2)
    await invoke { try await impl.f2(true) }
    // CHECK: Error: Empty()
    await invoke { try await impl.f2(false) }

    // CHECK: Success: (1, 2, 3)
    await invoke { try await impl.f3(true) }
    // CHECK: Error: Empty()
    await invoke { try await impl.f3(false) }

    // CHECK: Success: (1, 2, 3, 4)
    await invoke { try await impl.f4(true) }
    // CHECK: Error: Empty()
    await invoke { try await impl.f4(false) }

    // CHECK: Success: (1, 2, 3, 4, 5)
    await invoke { try await impl.f5(true) }
    // CHECK: Error: Empty()
    await invoke { try await impl.f5(false) }

    // CHECK: Success: ()
    await invoke { try await impl.g0(true) }
    // CHECK: Error: OneWord(x: 0)
    await invoke { try await impl.g0(false) }

    // CHECK: Success: 1
    await invoke { try await impl.g1(true) }
    // CHECK: Error: OneWord(x: 0)
    await invoke { try await impl.g1(false) }

    // CHECK: Success: (1, 2)
    await invoke { try await impl.g2(true) }
    // CHECK: Error: OneWord(x: 0)
    await invoke { try await impl.g2(false) }

    // CHECK: Success: (1, 2, 3)
    await invoke { try await impl.g3(true) }
    // CHECK: Error: OneWord(x: 0)
    await invoke { try await impl.g3(false) }

    // CHECK: Success: (1, 2, 3, 4)
    await invoke { try await impl.g4(true) }
    // CHECK: Error: OneWord(x: 0)
    await invoke { try await impl.g4(false) }

    // CHECK: Success: (1, 2, 3, 4, 5)
    await invoke { try await impl.g5(true) }
    // CHECK: Error: OneWord(x: 0)
    await invoke { try await impl.g5(false) }

    // CHECK: Success: ()
    await invoke { try await impl.h0(true) }
    // CHECK: Error: TwoWords(x: 0, y: 0)
    await invoke { try await impl.h0(false) }

    // CHECK: Success: 1
    await invoke { try await impl.h1(true) }
    // CHECK: Error: TwoWords(x: 0, y: 0)
    await invoke { try await impl.h1(false) }

    // CHECK: Success: (1, 2)
    await invoke { try await impl.h2(true) }
    // CHECK: Error: TwoWords(x: 0, y: 0)
    await invoke { try await impl.h2(false) }

    // CHECK: Success: (1, 2, 3)
    await invoke { try await impl.h3(true) }
    // CHECK: Error: TwoWords(x: 0, y: 0)
    await invoke { try await impl.h3(false) }

    // CHECK: Success: (1, 2, 3, 4)
    await invoke { try await impl.h4(true) }
    // CHECK: Error: TwoWords(x: 0, y: 0)
    await invoke { try await impl.h4(false) }

    // CHECK: Success: (1, 2, 3, 4, 5)
    await invoke { try await impl.h5(true) }
    // CHECK: Error: TwoWords(x: 0, y: 0)
    await invoke { try await impl.h5(false) }

    // CHECK: Success: ()
    await invoke { try await impl.i0(true) }
    // CHECK: Error: ThreeWords(x: 0, y: 0, z: 0)
    await invoke { try await impl.i0(false) }

    // CHECK: Success: 1
    await invoke { try await impl.i1(true) }
    // CHECK: Error: ThreeWords(x: 0, y: 0, z: 0)
    await invoke { try await impl.i1(false) }

    // CHECK: Success: (1, 2)
    await invoke { try await impl.i2(true) }
    // CHECK: Error: ThreeWords(x: 0, y: 0, z: 0)
    await invoke { try await impl.i2(false) }

    // CHECK: Success: (1, 2, 3)
    await invoke { try await impl.i3(true) }
    // CHECK: Error: ThreeWords(x: 0, y: 0, z: 0)
    await invoke { try await impl.i3(false) }

    // CHECK: Success: (1, 2, 3, 4)
    await invoke { try await impl.i4(true) }
    // CHECK: Error: ThreeWords(x: 0, y: 0, z: 0)
    await invoke { try await impl.i4(false) }

    // CHECK: Success: (1, 2, 3, 4, 5)
    await invoke { try await impl.i5(true) }
    // CHECK: Error: ThreeWords(x: 0, y: 0, z: 0)
    await invoke { try await impl.i5(false) }

    // CHECK: Success: (1.0, 2.0)
    await invoke { try await impl.nonMatching_f0(true) }
    // CHECK: Error: OneWord(x: 0)
    await invoke { try await impl.nonMatching_f0(false) }

    // CHECK: Success: (1.0, true, 2.0)
    await invoke { try await impl.nonMatching_f1(true) }
    // CHECK: Error: OneWord(x: 0)
    await invoke { try await impl.nonMatching_f1(false) }
}

enum SmallError: Error {
    case a(Int)
}

@inline(never)
func smallResultLargerError() throws(SmallError) -> Int8? {
  return 10
}

func callSmallResultLargerError() {
  let res = try! smallResultLargerError()
  print("Result is: \(String(describing: res))")
}

enum UInt8OptSingletonError: Error {
  case a(Int8?)
}

@inline(never)
func smallErrorLargerResult() throws(UInt8OptSingletonError) -> Int {
  throw .a(10)
}

func callSmallErrorLargerResult() {
  do {
    _ = try smallErrorLargerResult()
  } catch {
    switch error {
      case .a(let x):
        print("Error value is: \(String(describing: x))")
    }
  }
}

enum MyError: Error {
    case x
    case y
}

protocol AsyncGenProto<A> {
  associatedtype A
  func fn(arg: Int) async throws(MyError) -> A
}

@inline(never)
func callAsyncIndirectResult<A>(p: any AsyncGenProto<A>, x: Int) async throws(MyError) -> A {
  return try await p.fn(arg: x)
}


struct AsyncGenProtoImpl: AsyncGenProto {
    func fn(arg: Int) async throws(MyError) -> Int {
        print("Arg is \(arg)")
        return arg
    }
}

@main
public struct Main {
    public static func main() async {
        await checkSync()
        await checkAsync()
        // CHECK: Arg is 10
        print(try! await callAsyncIndirectResult(p: AsyncGenProtoImpl(), x: 10))

        // CHECK: Result is: Optional(10)
        callSmallResultLargerError()
        // CHECK: Error value is: Optional(10)
        callSmallErrorLargerResult()
    }
}
