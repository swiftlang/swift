// RUN: %target-build-swift %s -emit-module -emit-library -cxx-interoperability-mode=default -module-name OptionalLib -emit-module-path %t/artifacts/OptionalLib.swiftmodule -I %S/Inputs -O

// XFAIL: OS=linux-androideabi

import StdOptional
import CxxStdlib

extension ReturnsOptionalString {
    @inlinable
    public var string: String? {
        let _ = self.getStrDerived().pointee.map { $0 }
        return self.getStr().value.map {
            .init($0)
        }
    }
}
