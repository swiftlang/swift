// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %swift-interface-tool -action minimize-source -remove-internal-decls %t/input.swift > %t/output.swift
// RUN: %diff %t/output.swift %t/expected.swift

//--- input.swift
@usableFromInline
internal func usableFromInlineFunc() {
  print("ufil")
}

@_spi(Testing)
public func spiFunc() {
  print("spi")
}

package func packageFunc() {
  print("package")
}

internal func plainInternal() {
  print("plain")
}
//--- expected.swift
@usableFromInline
internal func usableFromInlineFunc()

@_spi(Testing)
public func spiFunc()

package func packageFunc()
