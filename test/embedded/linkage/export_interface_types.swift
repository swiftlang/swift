// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Library module

// RUN: %target-swift-frontend %t/Library.swift -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature Extern -emit-module-path %t/Library.swiftmodule -emit-ir -o - | %FileCheck -check-prefix LIBRARY-IR %s

// Application module

// RUN: %target-swift-frontend %t/Application.swift -I %t -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature Extern -emit-ir -o - | %FileCheck -check-prefix APPLICATION-IR %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Extern

//--- Library.swift

// @export(interface) struct: FullMetadata should be a strong public
// definition (ExternalLinkage), not linkonce_odr or private.
// LIBRARY-IR: @"$e7Library11ExportedFooVMf" = constant
@export(interface)
public struct ExportedFoo {
  public var x: Int
  public init(x: Int) { self.x = x }
}

// Non-@export(interface) struct: FullMetadata is lazily emitted per module;
// the Library module should NOT define it eagerly.
// LIBRARY-IR-NOT: @"$e7Library14NonExportedBarVMf"
public struct NonExportedBar {
  public var y: Int
  public init(y: Int) { self.y = y }
}

// @export(interface) class: FullMetadata should be a strong public definition.
// LIBRARY-IR: @"$e7Library13ExportedClassCMf" = constant
@export(interface)
public class ExportedClass {
  public var z: Int
  public init(z: Int) { self.z = z }
}

//--- Application.swift
import Library

// @export(interface) types: Application references their metadata from Library
// as external declarations, not local copies. External declarations appear
// before linkonce_odr definitions in the IR, so check them in IR order.
// APPLICATION-IR: @"$e7Library11ExportedFooVMf" = external global
// APPLICATION-IR: @"$e7Library13ExportedClassCMf" = external global

// Non-@export(interface) struct: Application emits its own linkonce_odr copy.
// APPLICATION-IR: @"$e7Library14NonExportedBarVMf" = linkonce_odr hidden constant

public func useExportedFoo() -> Any {
  return ExportedFoo(x: 42)
}

public func useNonExportedBar() -> Any {
  return NonExportedBar(y: 99)
}

public func useExportedClass() -> Any {
  return ExportedClass(z: 7)
}
