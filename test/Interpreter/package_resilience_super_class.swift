// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Access a package class (non-resilient type) subclassing a resilient class from a resilient module.
// RUN: %target-build-swift-dylib(%t/%target-library-name(Utils)) %t/Utils.swift -module-name Utils -package-name mypkg -emit-module -emit-module-path %t/Utils.swiftmodule -enable-library-evolution
// RUN: %target-build-swift -I %t -L %t -lUtils %s -o %t/main %target-rpath(%t) -package-name mypkg
// RUN: %target-run %t/main %t/%target-library-name(Utils) | %FileCheck %s

// RUN: rm -rf %t/%target-library-name(Utils)

/// Access a package class (non-resilient type) subclassing a resilient class from a non-resilient module.
// RUN: %target-build-swift-dylib(%t/%target-library-name(Utils)) %t/Utils.swift -module-name Utils -package-name mypkg -emit-module -emit-module-path %t/Utils.swiftmodule
// RUN: %target-build-swift -I %t -L %t -lUtils %s -o %t/main %target-rpath(%t) -package-name mypkg
// RUN: %target-run %t/main %t/%target-library-name(Utils) | %FileCheck %s

// RUN: rm -rf %t/%target-library-name(Utils)

/// Super class of the package class becomes non-resilient due to -enable-testing.
// RUN: %target-build-swift-dylib(%t/%target-library-name(Utils)) %t/Utils.swift -module-name Utils -package-name mypkg -emit-module -emit-module-path %t/Utils.swiftmodule -enable-library-evolution -enable-testing
// RUN: %target-build-swift -I %t -L %t -lUtils %s -o %t/main %target-rpath(%t) -package-name mypkg -DTESTABLE
// RUN: %target-run %t/main %t/%target-library-name(Utils) | %FileCheck %s

// UNSUPPORTED: OS=watchos
// UNSUPPORTED: OS=ios
// REQUIRES: executable_test

//--- Utils.swift

open class OpenKlass {
  public init() {}
  open var openVar = "open var"
}

package struct PkgStructWithEnum {
  package enum PkgEnum: String {
    case first = "First"
    case second = "Second"
  }
}

package class PkgKlass: OpenKlass {
  package var klassVar: PkgStructWithEnum.PkgEnum
  package override init() {
    klassVar = .second
  }
}


//--- main.swift

#if TESTABLE
@testable import Utils
#else
import Utils
#endif

public func f() {
  let k = PkgKlass()
  print(k.klassVar) // CHECK: second
  print("Value is", isEqual(k.klassVar, .second)) // CHECK: Value is true
}

public func isEqual<T:Equatable>(_ l: T, _ r: T) -> Bool {
  return l == r
}

f()
