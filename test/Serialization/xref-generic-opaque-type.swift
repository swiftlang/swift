// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/xref-opaque-generic-type/* %t/.
// RUN: cp %s %t/xref-generic-opaque-type.swift
// RUN: cd %t
// RUN: %target-swiftc_driver -emit-module -incremental %t/best-protocol.swift %t/xref-generic-opaque-type.swift -module-name A -output-file-map %t/output.json

@usableFromInline
struct GoodStruct: GoodProtocol {
  @usableFromInline
  typealias A = Int
}

extension BestProtocol {
  @inlinable
  @available(iOS 13.0, OSX 10.15, tvOS 13.0, watchOS 6.0, *)
  public func bestValue(_ x: Int) -> some BestProtocol {
    return _bestValue(GoodStruct.self, x)
  }
}
