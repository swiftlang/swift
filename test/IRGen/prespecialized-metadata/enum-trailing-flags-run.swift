// RUN: %empty-directory(%t)
// RUN: %clang -c -std=c++17 -v %target-cc-options %target-threading-opt -g -O0 -isysroot %sdk %S/Inputs/isPrespecialized.cpp -o %t/isPrespecialized.o -I %swift-include-dir -I %swift_src_root/include/ -I %swift_src_root/stdlib/public/SwiftShims/ -I %llvm_src_root/include -I %llvm_obj_root/include -L %swift-include-dir/../lib/swift/macosx

// RUN: %target-build-swift -v %mcp_opt %s %t/isPrespecialized.o -import-objc-header %S/Inputs/isPrespecialized.h -Xfrontend -prespecialize-generic-metadata -target %module-target-future -lc++ -L %swift-include-dir/../lib/swift/macosx -sdk %sdk -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main


// REQUIRES: OS=macosx
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: swift_test_mode_optimize
// UNSUPPORTED: swift_test_mode_optimize_size

func ptr<T>(to ty: T.Type) -> UnsafeMutableRawPointer {
    UnsafeMutableRawPointer(mutating: unsafePointerToMetadata(of: ty))!
}

func unsafePointerToMetadata<T>(of ty: T.Type) -> UnsafePointer<T.Type> {
  unsafeBitCast(ty, to: UnsafePointer<T.Type>.self)
}

protocol Natural {}

enum Zero : Natural {}

enum Successor<N : Natural> : Natural {}

extension Natural {
    static func withSuccessor<
        Invocation : WithSuccessorInvocation
    >
    (
        invoke invocation: Invocation
    )
        -> Invocation.Return
    {
        invocation.invoke(Successor<Self>.self)
    }

    static func withSuccessor<
        Invocation : WithSuccessorInvocation
    >
    (
        offsetBy count: UInt,
        invoke invocation: Invocation
    )
        -> Invocation.Return
    {
        if isCanonicalStaticallySpecializedGenericMetadata(ptr(to: Self.self)) {
            fatalError()
        }
        switch count {
        case 0:
            return invocation.invoke(Self.self)
        case 1:
            return withSuccessor(invoke: invocation)
        default:
            return Successor<Self>.withSuccessor(
                offsetBy: count - 1,
                invoke: invocation
            )
        }
    }
}

protocol WithSuccessorInvocation {
    associatedtype Return
    func invoke<N : Natural>( _ t: N.Type) -> Return
}

struct PointerInvocation : WithSuccessorInvocation {
    typealias Return = UnsafeMutableRawPointer
    func invoke<N : Natural>(_ t: N.Type) -> Return {
        ptr(to: t)
    }
}

allocateDirtyAndFreeChunk()

_ = Zero.withSuccessor(offsetBy: 10000, invoke: PointerInvocation())

