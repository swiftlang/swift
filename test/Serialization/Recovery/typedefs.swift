// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -emit-module -o %t -module-name Lib -I %S/Inputs/custom-modules %s

// RUN: %target-swift-ide-test -source-filename=x -print-module -module-to-print Lib -I %t -I %S/Inputs/custom-modules | %FileCheck %s

// RUN: %target-swift-ide-test -source-filename=x -print-module -module-to-print Lib -I %t -I %S/Inputs/custom-modules -Xcc -DBAD -enable-experimental-deserialization-recovery | %FileCheck -check-prefix CHECK-RECOVERY %s

// RUN: %target-swift-frontend -typecheck -I %t -I %S/Inputs/custom-modules -Xcc -DBAD -DTEST -enable-experimental-deserialization-recovery -DVERIFY %s -verify
// RUN: %target-swift-frontend -emit-silgen -I %t -I %S/Inputs/custom-modules -Xcc -DBAD -DTEST -enable-experimental-deserialization-recovery %s | %FileCheck -check-prefix CHECK-SIL %s

#if TEST

import Typedefs
import Lib

// CHECK-SIL-LABEL: sil hidden @_T08typedefs11testSymbolsyyF
func testSymbols() {
  // Check that the symbols are not using 'Bool'.
  // CHECK-SIL: function_ref @_T03Lib1xs5Int32Vfau
  _ = Lib.x
  // CHECK-SIL: function_ref @_T03Lib9usesAssocs5Int32VSgfau
  _ = Lib.usesAssoc
} // CHECK-SIL: end sil function '_T08typedefs11testSymbolsyyF'

#if VERIFY
let _: String = useAssoc(ImportedType.self) // expected-error {{cannot convert call result type '_.Assoc?' to expected type 'String'}}
let _: Bool? = useAssoc(ImportedType.self) // expected-error {{cannot convert value of type 'Int32?' to specified type 'Bool?'}}
let _: Int32? = useAssoc(ImportedType.self)

let _: String = useAssoc(AnotherType.self) // expected-error {{cannot convert call result type '_.Assoc?' to expected type 'String'}}
let _: Bool? = useAssoc(AnotherType.self) // expected-error {{cannot convert value of type 'AnotherType.Assoc?' (aka 'Optional<Int32>') to specified type 'Bool?'}}
let _: Int32? = useAssoc(AnotherType.self)
#endif // VERIFY

#else // TEST

import Typedefs

// CHECK-DAG: let x: MysteryTypedef
// CHECK-RECOVERY-DAG: let x: Int32
public let x: MysteryTypedef = 0

public protocol HasAssoc {
  associatedtype Assoc
}

extension ImportedType: HasAssoc {}

public struct AnotherType: HasAssoc {
  public typealias Assoc = MysteryTypedef
}

public func useAssoc<T: HasAssoc>(_: T.Type) -> T.Assoc? { return nil }

// CHECK-DAG: let usesAssoc: ImportedType.Assoc?
// CHECK-RECOVERY-DAG: let usesAssoc: Int32?
public let usesAssoc = useAssoc(ImportedType.self)
// CHECK-DAG: let usesAssoc2: AnotherType.Assoc?
// CHECK-RECOVERY-DAG: let usesAssoc2: AnotherType.Assoc?
public let usesAssoc2 = useAssoc(AnotherType.self)

#endif // TEST
