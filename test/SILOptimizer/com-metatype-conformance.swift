// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -emit-module-path %t/Library.swiftmodule -parse-as-library -module-name Library -enable-experimental-com-interop -I %t %t/Library.swift
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name Client -enable-experimental-com-interop -I %t %t/Client.swift -o /dev/null

// The optimizer deserializes and clones the always-inline generic body. Its
// substitution map contains the builtin conformance of `IWidget.Type` to
// `COMInterface`, which has no SIL witness table to deserialize.

//--- Library.swift
@com(interface: "10203040-5060-7080-90a0-b0c0d0e0f001")
public protocol IWidget: IUnknown { }

@inlinable
@inline(__always)
public func __uuidof<Interface>(_: Interface.Type) -> IID
    where Interface.Type: COMInterface {
  Interface.IID
}

//--- Client.swift
import Library

public func use() -> IID {
  __uuidof(IWidget.self)
}
