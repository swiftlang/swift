// RUN: %empty-directory(%t)

// RUN: %target-build-swift -c %s -DBASE -emit-library -emit-module -module-name Base -target %module-target-future -Xfrontend -prespecialize-generic-metadata -emit-module-path %t/Base.swiftmodule -o %t/%target-library-name(Base)
// RUN: %target-build-swift -c %s -DCONFORMANCE_1 -emit-library -emit-module -module-name Conformance1 -target %module-target-future -Xfrontend -prespecialize-generic-metadata -emit-module-path %t/Conformance1.swiftmodule -o %t/%target-library-name(Conformance1) -lBase -I %t -L %t
// RUN: %target-build-swift -c %s -DCONFORMANCE_2 -emit-library -emit-module -module-name Conformance2 -target %module-target-future -Xfrontend -prespecialize-generic-metadata -emit-module-path %t/Conformance2.swiftmodule -o %t/%target-library-name(Conformance2) -lBase -I %t -L %t
// RUN: %target-build-swift -c %s -DGENERIC -emit-library -emit-module -module-name Generic -target %module-target-future -Xfrontend -prespecialize-generic-metadata -emit-module-path %t/Generic.swiftmodule -o %t/%target-library-name(Generic) -lBase -lConformance1 -I %t -L %t
// RUN: %target-build-swift -c %s -DERASE -emit-library -emit-module -module-name Erase -target %module-target-future -Xfrontend -prespecialize-generic-metadata -emit-module-path %t/Erase.swiftmodule -o %t/%target-library-name(Erase) -lBase -lConformance2 -I %t -L %t
// RUN: %clang -c -std=c++17 -v %target-cc-options %target-threading-opt -g -O0 -isysroot %sdk %S/Inputs/isPrespecialized.cpp -o %t/isPrespecialized.o -I %swift-include-dir -I %swift_src_root/include/ -I %swift_src_root/stdlib/public/SwiftShims/ -I %llvm_src_root/include -I %llvm_obj_root/include -L %swift-include-dir/../lib/swift/macosx
// RUN: %target-build-swift %s %S/Inputs/main.swift %S/Inputs/consume-logging-metadata-value.swift %t/isPrespecialized.o -import-objc-header %S/Inputs/isPrespecialized.h -DMAIN -target %module-target-future -Xfrontend -prespecialize-generic-metadata -lBase -lConformance1 -lConformance2 -lGeneric -lErase -lc++ -I %t -L %t -L %swift-include-dir/../lib/swift/macosx -o %t/main

// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: swift_test_mode_optimize
// UNSUPPORTED: swift_test_mode_optimize_size
// UNSUPPORTED: remote_run

#if BASE
public struct K {
    public init() {}
}

public protocol P {
    var name: String { get }
}
#endif

#if CONFORMANCE_1
import Base

extension K : P {

    public var name: String { "Conformance1" }

}
#endif

#if CONFORMANCE_2
import Base

extension K : P {

    public var name: String { "Conformance2" }

}
#endif

#if GENERIC
import Base
import Conformance1

public struct G<T : P> {
    public let t: T

    public init(_ t: T) {
        self.t = t
    }

    public var name: String {
        t.name
    }
}

@inline(never)
func consume<T>(_ t: T) {
    withExtendedLifetime(t) {}
}


public func prespecialize() {
    consume(G<K>.self)
}
#endif

#if ERASE
import Base
import Conformance2

public struct AnyP : P {

    private class _BoxBase {
        public var name: String {
            fatalError()
        }
        public func visit<Visitor : PVisitor>(_ v: Visitor) {
            fatalError()
        }
    }

    private final class _Box<T : P> : _BoxBase {
        private let t: T

        init(_ t: T) {
            self.t = t
        }

        override var name: String {
            t.name
        }
        override func visit<Visitor : PVisitor>(_ v: Visitor) {
            v.visit(t)
        }
    }

    private let _box: _BoxBase

    init<T : P>(_ t: T) {
        self._box = _Box(t)
    }

    public var name: String {
        _box.name
    }

    public func visit<Visitor : PVisitor>(_ v: Visitor) {
        _box.visit(v)
    }

}

public protocol PVisitor {
    func visit<T : P>(_ t: T)
}

public func getKAsAnyP() -> AnyP {
    AnyP(K())
}
#endif

#if MAIN
import Base
import Generic
import Conformance2
import Erase

func ptr<T>(to ty: T.Type) -> UnsafeMutableRawPointer {
    UnsafeMutableRawPointer(mutating: unsafePointerToMetadata(of: ty))!
}

func printTheName<T : P>(_ t: T, prespecialized: Bool) {
    print(G(t).name)
    assert(isCanonicalStaticallySpecializedGenericMetadata(ptr(to: G<T>.self)) == prespecialized)
}
struct Visitor : PVisitor {
    func visit<T : P>(_ t: T) {
        printTheName(t, prespecialized: false)
    }
}
func doit() {
  // CHECK: Conformance1
  printTheName(K(), prespecialized: true)
  // CHECK: Conformance2
  getKAsAnyP().visit(Visitor())
}
#endif
