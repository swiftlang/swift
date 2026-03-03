// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

// RUN: %target-swift-frontend -emit-module %t/src/A.swift \
// RUN:   -module-name A \
// RUN:   -emit-module-path %t/A.swiftmodule

// RUN: %target-swift-frontend -emit-module %t/src/B.swift \
// RUN:   -module-name B \
// RUN:   -emit-module-path %t/B.swiftmodule

// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated -module-name main -I %t %t/src/main.swift

//--- A.swift
public struct Bag {
}

//--- B.swift
public struct Bag {
}

//--- main.swift
import A
import B

protocol P {
}

struct Test {
    func inject<T>(_: T.Type = T.self) {}

    func inject<T: P>(_: T) {}

    func inject<T>(_: T) async -> T {}
}

func test(t: Test) {
    t.inject(Bag.self)  // expected-error {{ambiguous use of 'Bag'}}
}
