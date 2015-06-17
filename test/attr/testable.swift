// RUN: rm -rf %t && mkdir %t
// RUN: %target-swift-frontend -emit-module -enable-testing -o %t %S/../Inputs/empty.swift
// RUN: %target-swift-frontend -parse -I %t -I %S/Inputs/custom-modules %s -verify
// RUN: %target-swift-frontend -parse -I %t -I %S/Inputs/custom-modules %s -disable-testable-attr-requires-testable-module -DIMPORTS_ONLY

@testable import Swift // expected-error {{module 'Swift' was not compiled for testing}}
@testable import empty // no-error
@testable import Testable_ClangModule // no-error

_ = clangGlobal

#if !IMPORTS_ONLY
@testable func foo() {} // expected-error {{@testable may only be used on 'import' declarations}}
#endif
