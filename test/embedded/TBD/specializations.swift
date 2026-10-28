// Some specializations created by the mandatory optimizations of Embedded
// Swift are public. Any module that uses the generic function can emit the
// same specialization, so none of them are strongly defined, and the TBD
// matches the IR.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-ir -o %t/Library.ll %t/Library.swift -parse-as-library -module-name Library -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -validate-tbd-against-ir=all
// RUN: %FileCheck -check-prefix LIBRARY-IR %s < %t/Library.ll
// RUN: %target-swift-frontend -emit-ir -o /dev/null %t/Library.swift -parse-as-library -module-name Library -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -validate-tbd-against-ir=all -O
// RUN: %target-swift-frontend -emit-ir -o /dev/null %t/Library.swift -parse-as-library -module-name Library -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=implementation -validate-tbd-against-ir=all

// Clients emit their own copies of the specializations they use.
// RUN: %target-swift-frontend -c -emit-module -o %t/Library.o %t/Library.swift -parse-as-library -module-name Library -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface
// RUN: %target-swift-frontend -c -I %t -o %t/Application.o %t/Application.swift -parse-as-library -module-name Application -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/Library.o %t/Application.o -o %t/Application
// RUN: %target-run %t/Application | %FileCheck -check-prefix OUTPUT %s

// The same, when the library may drop its own unused copies.
// RUN: %target-swift-frontend -c -emit-module -o %t/Library.o %t/Library.swift -parse-as-library -module-name Library -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=implementation -O
// RUN: %target-swift-frontend -c -I %t -o %t/Application.o %t/Application.swift -parse-as-library -module-name Application -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=implementation -O
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/Library.o %t/Application.o -o %t/Application
// RUN: %target-run %t/Application | %FileCheck -check-prefix OUTPUT %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded

//--- Library.swift

// The specialization of a default implementation, which is called from the
// serialized witness thunk of the conformance.
// LIBRARY-IR-DAG: define linkonce_odr {{(protected )?}}{{.*}}@"$e7Library8DescribePAAE8describeSiyFAA6ThingyV_Tgq5"(
public protocol Describe {
  func describe() -> Int
}

extension Describe {
  public func describe() -> Int { 42 }
}

public struct Thingy: Describe {
  public init() {}
}

public func makeDescribe() -> any Describe { Thingy() }

// The specializations of a generic class's methods for its specialized vtable.
// LIBRARY-IR-DAG: define linkonce_odr {{(protected )?}}{{.*}}@"$e7Library3BoxC3getxyFSi_Tg5"(
// LIBRARY-IR-DAG: define linkonce_odr {{(protected )?}}{{.*}}@"$e7Library3BoxC5valuexvgSi_Tgq5"(
// LIBRARY-IR-DAG: define linkonce_odr {{(protected )?}}{{.*}}@"$e7Library3BoxCfDSi_Tg5"(
open class Box<T> {
  public var value: T
  public init(_ value: T) { self.value = value }
  open func get() -> T { value }
}

public func makeIntBox() -> Box<Int> { Box(17) }

//--- Application.swift
import Library

struct Local: Describe {}

@main
struct Main {
  static func main() {
    print(makeDescribe().describe())
    let local: any Describe = Thingy()
    print(local.describe())
    print(Local().describe())
    let b = makeIntBox()
    print(b.get())
    let c = Box<Int>(5)
    print(c.get())
    print(c.value)
  }
}

// OUTPUT: 42
// OUTPUT-NEXT: 42
// OUTPUT-NEXT: 42
// OUTPUT-NEXT: 17
// OUTPUT-NEXT: 5
// OUTPUT-NEXT: 5
