/// Ensure that export_as decls don't cause a deserialization failure (even one if recovered from)
/// rdar://90272035

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -DLIB_A %s -module-name A -emit-module-path %t/A.swiftmodule -I %t -I %S/Inputs/exported-modules
// RUN: %target-swift-frontend -emit-module -DLIB_B %s -module-name B -emit-module-path %t/B.swiftmodule -I %t -I %S/Inputs/exported-modules -disable-deserialization-recovery

#if LIB_A
import ExportedCore

public func foo() -> ExportedType { fatalError() }

#elseif LIB_B

import A

let a = foo()

#endif
