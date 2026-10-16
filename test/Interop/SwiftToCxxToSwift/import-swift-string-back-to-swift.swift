// RUN: %empty-directory(%t)
// RUN: %target-swiftxx-frontend %S/Inputs/swift-string.swift -module-name StringBridge \
// RUN:   -typecheck -emit-clang-header-path %t/StringBridge.h
// RUN: %target-swift-ide-test -print-module -module-to-print=SwiftStringRoundTrip \
// RUN:   -source-filename=%s -I %t -Xcc -fmodule-map-file=%S/Inputs/swift-string.modulemap \
// RUN:   -cxx-interoperability-mode=default -Xcc -DSWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR \
// RUN:   -enable-experimental-feature CxxSwiftValueTypes | %FileCheck %s --check-prefix=INTERFACE
// RUN: %target-swiftxx-frontend %s -emit-silgen -I %t \
// RUN:   -Xcc -fmodule-map-file=%S/Inputs/swift-string.modulemap -Xcc -DSWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR \
// RUN:   -enable-experimental-feature CxxSwiftValueTypes | %FileCheck %s --check-prefix=SIL
// RUN: %target-swiftxx-frontend %s -emit-ir -I %t \
// RUN:   -Xcc -fmodule-map-file=%S/Inputs/swift-string.modulemap -Xcc -DSWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR \
// RUN:   -enable-experimental-feature CxxSwiftValueTypes | %FileCheck %s --check-prefix=IR
// RUN: %target-swiftxx-frontend %s -typecheck -verify -DNO_USE -I %t \
// RUN:   -Xcc -fmodule-map-file=%S/Inputs/swift-string.modulemap -Xcc -DSWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR
// RUN: %target-swiftxx-frontend %s -typecheck -verify -DNO_USE -I %t \
// RUN:   -Xcc -fmodule-map-file=%S/Inputs/swift-string.modulemap -Xcc -DSWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR \
// RUN:   -enable-experimental-feature CxxSwiftValueTypes

// REQUIRES: swift_feature_CxxSwiftValueTypes

import SwiftStringRoundTrip

// INTERFACE: func hello(_ string: String)
// INTERFACE: func makeString() -> String
// INTERFACE: func echoString(_ string: String) -> String
// INTERFACE: func borrowString(_ string: String) -> String
// INTERFACE: func mutateCopy(_ string: String) -> String
// INTERFACE: func mutateString(_ string: inout String)
// INTERFACE: func consumeString(consuming string: consuming String) -> String
// INTERFACE: func borrowConstRValue(consuming string: String) -> String
// INTERFACE: func echoAlias(_ string: String) -> String
// INTERFACE: enum Strings {
// INTERFACE: static func echo(_ string: String) -> String
// INTERFACE: struct StringFunctions {
// INTERFACE: func echo(_ string: String) -> String
// INTERFACE: static func make() -> String

func checkUnrelated(_ value: SwiftStringRoundTrip.String) -> SwiftStringRoundTrip.String {
  echoUnrelatedString(value)
}

func checkTrivial(_ value: TrivialString) -> TrivialString {
  echoTrivialString(value)
}

func checkWrongModule(_ value: WrongModuleString) -> WrongModuleString {
  echoWrongModuleString(value)
}

func checkNonGenerated(_ value: NonGeneratedString) -> NonGeneratedString {
  echoNonGeneratedString(value)
}

func checkWrongUSR(_ value: WrongUSRString) -> WrongUSRString {
  echoWrongUSRString(value)
}

func checkOpaque(_ value: OpaqueString) -> OpaqueString {
  echoOpaqueString(value)
}

#if !NO_USE
func takeString(_ value: Swift.String) {
  hello(value)
}

func returnString() -> Swift.String {
  makeString()
}

func mutate(_ value: inout Swift.String) {
  mutateString(&value)
}

// SIL-LABEL: sil hidden {{.*}}takeString
// SIL: function_ref {{.*}} : $@convention(c) (@in{{(_cxx)?}} String) -> ()
// SIL: apply {{.*}} : $@convention(c) (@in{{(_cxx)?}} String) -> ()
// SIL-LABEL: sil hidden {{.*}}returnString
// SIL: function_ref {{.*}} : $@convention(c) () -> @owned String
// SIL: apply {{.*}} : $@convention(c) () -> @owned String
// SIL-LABEL: sil hidden {{.*}}mutate
// SIL: function_ref {{.*}} : $@convention(c) (@inout String) -> ()
// SIL: apply {{.*}} : $@convention(c) (@inout String) -> ()

// IR-LABEL: define {{.*}}takeString
// IR: {{call|invoke}} void @{{.*}}hello{{.*}}(ptr
// IR-LABEL: define {{.*}}returnString
// IR: {{call|invoke}} void @{{.*}}makeString{{.*}}(ptr {{.*}}sret(%TSS)
// IR-LABEL: define {{.*}}mutate
// IR: {{call|invoke}} void @{{.*}}mutateString{{.*}}(ptr
#endif
