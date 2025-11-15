// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

// Note: IRGen uses internal linkage instead of linkonce_odr only for COFF for dead stub methods.
// UNSUPPORTED: OS=windows-msvc

// Ensure that swift_dead_method_stub is emitted without comdat linkage
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -parse-as-library -O -module-name A -c -primary-file %t/A.swift %t/B.swift -emit-ir -o - | %FileCheck %s -check-prefix CHECK
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -parse-as-library -O -module-name A -c %t/A.swift -primary-file %t/B.swift -emit-ir -o - | %FileCheck %s -check-prefix CHECK

// CHECK-LABEL: define {{(linkonce_odr )?}}hidden void @_swift_dead_method_stub(
// CHECK-NOT: comdat
// CHECK: {


// Ensure that link-time deduplication for swift_dead_method_stub works correctly
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -parse-as-library -O -module-name A -c -primary-file %t/A.swift %t/B.swift -o %t/A.swift.o
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -parse-as-library -O -module-name A -c %t/A.swift -primary-file %t/B.swift -o %t/B.swift.o
// RUN: %target-build-swift -target %target-swift-5.1-abi-triple %t/B.swift.o %t/A.swift.o -o %t/a.out

//--- A.swift

// Define an open class with a dead vtable entry
class C1 {
  private func dead() {}
}

//--- B.swift

class C2: C1 {
  // Define another dead vtable entry to ensure that this object file
  // also should have dead vtable stub definition
  private func beef() {}
}

@main
struct Entry {
    static func main() {
        _ = C2()
    }
}

