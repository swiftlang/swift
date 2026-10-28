// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module %t/Lib.swift -parse-as-library -module-name Lib -emit-module-path %t/Lib.swiftmodule -emit-module-source-info-path %t/Lib.swiftsourceinfo
// RUN: %target-swift-frontend -enable-sil-opaque-values -parse-as-library -emit-sil -O -module-name M -I %t -save-optimization-record=yaml -save-optimization-record-path %t/remarks.yaml %t/Client.swift -o %t/M.sil
// RUN: %FileCheck --input-file=%t/remarks.yaml %s
// RUN: %FileCheck --check-prefix=SIL --input-file=%t/M.sil %s

// The remark generator looks up the location of the UsesDefault witness thunk's
// $return_value ParamDecl. Its DeclContext is Lib's default make(), deserialized
// from Lib.swiftmodule with Lib.swiftsourceinfo.

// CHECK:      Function: '$s1M10OwnWitnessV4makexyF'
// CHECK:      InferredValue: 'to location ''$return_value'''

// SIL-LABEL: sil private {{.*}}@$s1M11UsesDefaultVyxG3Lib5MakerAaeFP4make4MadeQzyFTW :
// SIL-NOT:   } // end sil function
// SIL:       {{^}}// %0 "$return_value"
// SIL-NOT:   } // end sil function
// SIL:       copy_addr %1 to [init] %0

//--- Lib.swift
public protocol Maker {
  associatedtype Made
  func make() -> Made
}

extension Maker where Made == Self {
  // Inlined so the thunk copies into its indirect result, which the remark
  // generator visits.
  @inlinable @inline(__always)
  public func make() -> Self { self }
}

//--- Client.swift
import Lib

struct UsesDefault<T>: Maker {
  typealias Made = UsesDefault<T>
  var t: T
}

struct OwnWitness<T>: Maker {
  var t: T
  func make() -> T { t }
}
