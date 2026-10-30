// RUN: %empty-directory(%t)
// RUN: %target-build-swift -target %target-swift-5.7-abi-triple %s -parse-as-library -emit-module -emit-library %no-fixup-chains -module-name SymbolicShape -o %t/%target-library-name(TypesToReflect)
// RUN: %target-swift-reflection-dump %t/%target-library-name(TypesToReflect) | %FileCheck %s

// REQUIRES: no_asan

// LC_DYLD_CHAINED_FIXUPS decode not currently supported (default on visionOS)
// UNSUPPORTED: OS=xros


// Symbolic references to extended existential shapes are resolved to remote
// addresses rather than indices; printing them must not read the index
// payload.

public protocol P {}
public protocol PP<T> { associatedtype T }

public struct S<T> {
  public let field: (any PP<T> & P)?
}

// CHECK: SymbolicShape.S
// CHECK: field: Swift.Optional<symbolic existential type ({{(non-)?}}unique) 0x{{[0-9A-F]+}} <A>>
