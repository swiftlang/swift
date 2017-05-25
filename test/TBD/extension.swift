// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift %S/Inputs/extension_types.swift -module-name ExtensionTypes -emit-module -emit-module-path %t/ExtensionTypes.swiftmodule
// RUN: %target-swift-frontend -emit-ir -o- -parse-as-library -module-name test -validate-tbd-against-ir=all -I %t %s

import ExtensionTypes

public protocol Public {}
internal protocol Internal {}
private protocol Private {}

extension ForeignStruct: Foreign {}
extension ForeignStruct: Public {}
extension ForeignStruct: Internal {}
extension ForeignStruct: Private {}
extension ForeignStruct2: Foreign, Public, Internal, Private {}

public struct PublicStruct {}
public struct PublicStruct2 {}
internal struct InternalStruct {}
internal struct InternalStruct2 {}
private struct PrivateStruct {}
private struct PrivateStruct2 {}

extension PublicStruct: Foreign {}
extension PublicStruct: Public {}
extension PublicStruct: Internal {}
extension PublicStruct: Private {}
extension PublicStruct2: Foreign, Public, Internal, Private {}

extension InternalStruct: Foreign {}
extension InternalStruct: Public {}
extension InternalStruct: Internal {}
extension InternalStruct: Private {}
extension InternalStruct2: Foreign, Public, Internal, Private {}

extension PrivateStruct: Foreign {}
extension PrivateStruct: Public {}
extension PrivateStruct: Internal {}
extension PrivateStruct: Private {}
extension PrivateStruct2: Foreign, Public, Internal, Private {}
