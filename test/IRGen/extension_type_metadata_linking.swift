// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -c  %S/Inputs/simple.swift %s -O  -num-threads 1 -o %t/simple.o -o %t/extension_type_metadata_linking.o
// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -c  %S/Inputs/main.swift -o %t/main.o
// RUN: %target-build-swift %t/extension_type_metadata_linking.o %t/simple.o %t/main.o -o %t/main.out

// REQUIRES: objc_interop

// Check that type metadata defined inside extensions of files imported from
// other modules is emitted properly and there no linking errors.
// In particular, it should be possible to define types inside extensions of
// types imported from Foundation (rdar://problem/27245620).

import Foundation

extension NSNumber {
    class Base : CustomStringConvertible {
        public var description: String {
            return "Base"
        }
    }

    class Derived: Base {
        override public var description: String {
            return "Derived"
        }
    }
}

