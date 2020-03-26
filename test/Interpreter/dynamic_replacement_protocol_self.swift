// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(TestModuleLinking)) -module-name TestModuleLinking -emit-module -emit-module-path %t/TestModuleLinking.swiftmodule -swift-version 5 %S/Inputs/dynamic_replacement_protocol_self_orig.swift -Xfrontend -enable-private-imports -Xfrontend -enable-implicit-dynamic
// RUN: %target-build-swift -I%t -L%t -lTestModuleLinking -o %t/main %target-rpath(%t) %s -swift-version 5
// RUN: %target-codesign %t/main %t/%target-library-name(TestModuleLinking)
// RUN: %target-run %t/main %t/%target-library-name(TestModuleLinking) %t/%target-library-name(TestModuleLinking)

// N.B. We're not actually executing anything here - all we care about is
// if the linker is content.

// REQUIRES: executable_test

// UNSUPPORTED: swift_test_mode_optimize_none_with_implicit_dynamic

@_private(sourceFile: "dynamic_replacement_protocol_self_orig.swift") import TestModuleLinking

extension NewUserModel {
  @_dynamicReplacement(for: view) private var __preview__view: AnyView {
    Self.view(for: self, ofType: NewUserView.self)
  }
}

typealias NewUserModel = TestModuleLinking.NewUserModel
