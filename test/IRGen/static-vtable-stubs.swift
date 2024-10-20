// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -parse-as-library -static -O -module-name M -c -primary-file %t/A.swift %t/B.swift -S -emit-ir -o - | %FileCheck %t/A.swift -check-prefix CHECK
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -parse-as-library -static -O -module-name M -c %t/A.swift -primary-file %t/B.swift -S -emit-ir -o - | %FileCheck %t/B.swift -check-prefix CHECK

// Verify that we can link successfully.
// RUN: %target-build-swift -target %target-swift-5.1-abi-triple -O %t/A.swift %t/B.swift -o %t/a.out

// REQUIRES: concurrency

//--- A.swift
open class C {
  private var i: [ObjectIdentifier:Any] = [:]

  private func foo() async {}
}

// CHECK: @"$s1M1CC3foo33_{{.*}}Tu" = hidden global %swift.async_func_pointer <{ {{.*}} @"$s1M1CC1i33_807E3D81CC6CDD898084F3279464DDF9LLSDySOypGvg"

// CHECK: @"$s1M1CC1i33_807E3D81CC6CDD898084F3279464DDF9LLSDySOypGvs" = hidden alias void (), ptr @"$s1M1CC1i33_807E3D81CC6CDD898084F3279464DDF9LLSDySOypGvg"
// CHECK: @"$s1M1CC1i33_807E3D81CC6CDD898084F3279464DDF9LLSDySOypGvM" = hidden alias void (), ptr @"$s1M1CC1i33_807E3D81CC6CDD898084F3279464DDF9LLSDySOypGvg"

// CHECK: define hidden void @"$s1M1CC1i33_807E3D81CC6CDD898084F3279464DDF9LLSDySOypGvg"()
// CHECK: entry:
// CHECK:   tail call void @swift_deletedMethodError()

//--- B.swift
final class D: C {
}

// CHECK: declare swiftcc ptr @"$s1M1CC1i33_807E3D81CC6CDD898084F3279464DDF9LLSDySOypGvg"(ptr swiftself) #0
// CHECK: declare swiftcc void @"$s1M1CC1i33_807E3D81CC6CDD898084F3279464DDF9LLSDySOypGvs"(ptr, ptr swiftself) #0
// CHECK: declare swiftcc { ptr, ptr } @"$s1M1CC1i33_807E3D81CC6CDD898084F3279464DDF9LLSDySOypGvM"(ptr noalias dereferenceable({{32|16}}), ptr swiftself) #0

@main
struct Main {
  static func main() {
  }
}

