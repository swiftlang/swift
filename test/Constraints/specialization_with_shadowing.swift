// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

/// Build the library A
// RUN: %target-swift-frontend -emit-module %t/src/A.swift \
// RUN:   -module-name A -swift-version 5 -enable-library-evolution \
// RUN:   -emit-module-path %t/A.swiftmodule \
// RUN:   -emit-module-interface-path %t/A.swiftinterface

/// Build the library B
// RUN: %target-swift-frontend -emit-module %t/src/B.swift \
// RUN:   -module-name B -swift-version 5 -enable-library-evolution \
// RUN:   -emit-module-path %t/B.swiftmodule \
// RUN:   -emit-module-interface-path %t/B.swiftinterface

// Build the client AB (language mode 5)
// RUN: %target-swift-frontend -emit-module %t/src/ClientAB.swift \
// RUN:   -module-name Client -I %t -swift-version 5 \
// RUN:   -verify

// Build the client AB (language mode 6)
// RUN: %target-swift-frontend -emit-module %t/src/ClientAB.swift \
// RUN:   -module-name Client -I %t -swift-version 6 \
// RUN:   -verify

// Build the client BA (language mode 5)
// RUN: %target-swift-frontend -emit-module %t/src/ClientBA.swift \
// RUN:   -module-name Client -I %t -swift-version 5 \
// RUN:   -verify

// Build the client BA (language mode 6)
// RUN: %target-swift-frontend -emit-module %t/src/ClientBA.swift \
// RUN:   -module-name Client -I %t -swift-version 6 \
// RUN:   -verify

//--- A.swift
public protocol EventSource {
}

//--- B.swift
public struct Event<T> {
}

public class EventSource<Parameter> {
    public var event: Event<Parameter> {
        fatalError()
    }

    public init() {}
}

//--- ClientAB.swift
import B
// Note: import order is important in this case because successful match might
//       mean that the other overload is not going to be attempted and we want
//       to attempt protocol EventSource always.
import A

func test() {
    let test: B.Event<Void>
    test = EventSource<Void>().event
    print(test)
}

//--- ClientBA.swift
import B
// Note: import order is important in this case because successful match might
//       mean that the other overload is not going to be attempted and we want
//       to attempt protocol EventSource always.
import A

func test() {
    let test: B.Event<Void>
    test = EventSource<Void>().event
    print(test)
}
