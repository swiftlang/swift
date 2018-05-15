// RUN: %swift -module-name test -target x86_64-apple-macosx10.9  -emit-ir -parse-stdlib -emit-public-type-metadata-accessors -primary-file %s %S/Inputs/emit_public_type_metadata_accessors_other.swift | %FileCheck --check-prefix=CHECK-PUBLIC %s

// RUN: %swift -module-name test -target x86_64-apple-macosx10.9  -emit-ir -parse-stdlib -primary-file %s %S/Inputs/emit_public_type_metadata_accessors_other.swift | %FileCheck --check-prefix=CHECK-NONPUBLIC %s

private class C { }

// CHECK-PUBLIC: define swiftcc %swift.metadata_response @"$S4test3Foo33_DEC9477CC6E8E6E7A9CE422B1DBE7EA4LLVMa"
// CHECK-NONPUBLIC: define internal swiftcc %swift.metadata_response @"$S4test3Foo33_DEC9477CC6E8E6E7A9CE422B1DBE7EA4LLVMa"
private struct Foo {
  private let c: C = C()
}

public struct Wrapper {
  private let foo: Foo = Foo()
}


