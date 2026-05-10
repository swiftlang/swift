// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -O -emit-module -emit-module-path %t/Library.swiftmodule -parse-as-library %t/Library.swift -enable-library-evolution
// RUN: %target-swift-frontend -O -emit-sil %t/Client.swift -I %t -module-name test | %FileCheck %s

// UNSUPPORTED: OS=windows-msvc

//--- Library.swift

@usableFromInline func usableFromInlineFunc() {}

public struct S {
  @usableFromInline var member: Int = 0
  public init() {}
}

@_alwaysEmitIntoClient
public func serializedHasSymbolFunc(_ s: S) -> Bool {
  guard #_hasSymbol(usableFromInlineFunc) else { return false }
  guard #_hasSymbol(s.member) else { return false }
  return true
}

//--- Client.swift

@_weakLinked import Library

public func foo() -> Bool {
  // CHECK:   {{%[0-9]+}} = has_symbol #usableFromInlineFunc
  // CHECK:   {{%[0-9]+}} = has_symbol #S.member
  return serializedHasSymbolFunc(S())
}

// Verify that the functions referenced by the deserialized `has_symbol`
// instructions have also been deserialized and remain after dead function
// elimination.

// CHECK: sil @$s7Library20usableFromInlineFuncyyF : $@convention(thin) () -> ()
// CHECK: sil @$s7Library1SV6memberSivg : $@convention(method) (@in_guaranteed S) -> Int
// CHECK: sil @$s7Library1SV6memberSivs : $@convention(method) (Int, @inout S) -> ()
// CHECK: sil @$s7Library1SV6memberSivM : $@yield_once @convention(method) (@inout S) -> @yields @inout Int
