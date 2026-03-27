// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %swift-interface-tool -action minimize-source -remove-internal-decls %t/input.swift > %t/output.swift
// RUN: %diff %t/output.swift %t/expected.swift
// RUN: %swift-interface-tool -action minimize-source -remove-internal-decls -internal-import-by-default %t/input.swift > %t/output-internal.swift
// RUN: %diff %t/output-internal.swift %t/expected-internal.swift
// RUN: %swift-interface-tool -action minimize-source %t/input.swift > %t/output-default.swift
// RUN: %diff %t/output-default.swift %t/expected-default.swift

//--- input.swift
import Foundation
public import PublicMod
package import PackageMod
internal import InternalMod
private import PrivateMod
fileprivate import FileprivateMod
open import OpenMod
@_exported import ExportedMod
@_implementationOnly import HiddenMod
@_spi(Testing) import SPIMod
//--- expected.swift
import Foundation
public import PublicMod
package import PackageMod
private import PrivateMod
open import OpenMod
@_exported import ExportedMod
@_implementationOnly import HiddenMod
@_spi(Testing) import SPIMod
//--- expected-internal.swift

public import PublicMod
package import PackageMod
private import PrivateMod
open import OpenMod
@_exported import ExportedMod
@_implementationOnly import HiddenMod
//--- expected-default.swift
import Foundation
public import PublicMod
package import PackageMod
internal import InternalMod
private import PrivateMod
fileprivate import FileprivateMod
open import OpenMod
@_exported import ExportedMod
@_implementationOnly import HiddenMod
@_spi(Testing) import SPIMod
