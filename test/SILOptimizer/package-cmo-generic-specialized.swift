// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-build-swift %t/Lib.swift \
// RUN: -module-name=Lib -package-name Pkg \
// RUN: -Xfrontend -experimental-allow-non-resilient-access \
// RUN: -Xfrontend -experimental-package-cmo \
// RUN: -O -wmo -enable-library-evolution \
// RUN: -emit-sil -Xllvm -sil-print-functions=ToggleState -o %t/Lib-passes.txt

// RUN: %FileCheck %s

// REQUIRES: swift_in_compiler

//--- main.swift

import Lib
// CHECK: sadf

//--- Lib.swift

package enum ToggleState: UInt {
    case on
    case off
    case mixed

    package init(_ isOn: Bool) {
        self = isOn ? .on : .off
    }

    package static func stateFor<T: Equatable, C: Collection>(
        item: T, in collection: C
    ) -> ToggleState where C.Element == Binding<T> {
        collection.allSatisfy(
            { item == $0._value }) ? .on :
            collection.allSatisfy(
                { item != $0._value }) ? .off : .mixed
    }
}


@frozen
public struct Binding<Value> {
    package var _value: Value
    package init(value: Value) {
        _value = value
    }
}
