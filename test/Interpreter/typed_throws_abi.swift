// RUN: %target-build-swift-dylib(%t/%target-library-name(TypedThrowsABI)) -enable-library-evolution %S/Inputs/typed_throws_abi_impl.swift -emit-module -emit-module-path %t/TypedThrowsABI.swiftmodule -module-name TypedThrowsABI

// RUN: %target-build-swift -parse-as-library -Xfrontend -disable-availability-checking %s -lTypedThrowsABI -I %t -L %t -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main %t/%target-library-name(TypedThrowsABI) | %FileCheck %s

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
}

@main
public struct Main {
    public static func main() async {
        await checkSync()
        await checkAsync()
    }
}
