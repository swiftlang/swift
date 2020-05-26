// RUN: %target-typecheck-verify-swift

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/abi %s -emit-ir | %FileCheck %s

// REQUIRES: objc_interop
import Foundation

@objc protocol JSExport { }

@objc protocol RootJSExport : JSExport { }

// CHECK: @_PROTOCOL_PROTOCOLS__TtP8JSExport4Sub1_ = private constant{{.*}}_PROTOCOL__TtP8JSExport8JSExport_{{.*}}_PROTOCOL__TtP8JSExport12RootJSExport_
@objc protocol Sub1 : JSExport, RootJSExport { }

// CHECK: @_PROTOCOL_PROTOCOLS__TtP8JSExport4Sub2_{{.*}}_PROTOCOL__TtP8JSExport4Sub1_{{.*}}_PROTOCOL__TtP8JSExport8JSExport_
@objc protocol Sub2 : Sub1, JSExport { }

// CHECK: @_PROTOCOL_PROTOCOLS__TtP8JSExport4Sub3_ = private constant{{.*}}@_PROTOCOL__TtP8JSExport4Sub2_
@objc protocol Sub3 : Sub2 { }

protocol ReexportJSExport : RootJSExport, JSExport { }
