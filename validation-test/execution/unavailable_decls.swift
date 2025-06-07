// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-build-swift-dylib(%t/%target-library-name(Library)) -emit-module -emit-module-path %t/Library.swiftmodule -module-name Library -parse-as-library %t/Library.swift -swift-version 5 -enable-library-evolution
// RUN: %target-build-swift-dylib(%t/%target-library-name(ClientLibrary)) %t/ClientA.swift %t/ClientB.swift -I%t -L%t -swift-version 5 -lLibrary

//--- Library.swift

@available(*, unavailable)
public struct UnavailableStruct {}

//--- ClientA.swift

import Library

@inline(never)
func blackHole<T>(_ t: T) {}

@available(*, unavailable)
public func foo() {
  blackHole(UnavailableStruct.self)
}

//--- ClientB.swift

import Library

@available(*, unavailable)
public func bar() {
  blackHole(UnavailableStruct.self)
}
