// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %swift-interface-tool -action minimize-source %t/input.swift > %t/output.swift
// RUN: %diff %t/output.swift %t/expected.swift
// RUN: %swift-interface-tool -action minimize-source -internal-import-by-default %t/input.swift > %t/output-internal.swift
// RUN: %diff %t/output-internal.swift %t/expected-internal.swift

//--- input.swift
import Foundation
public import PublicMod
package import PackageMod
internal import InternalMod
private import PrivateMod
fileprivate import FileprivateMod
@_implementationOnly import HiddenMod
@_spi(Testing) import SPIMod
//--- expected.swift
import Foundation
public import PublicMod
package import PackageMod
private import PrivateMod
@_implementationOnly import HiddenMod
@_spi(Testing) import SPIMod
//--- expected-internal.swift

public import PublicMod
package import PackageMod
private import PrivateMod
@_implementationOnly import HiddenMod
