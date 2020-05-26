// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -parse-as-library %S/Inputs/overlay.swift -sdk %S/Inputs -I %S/Inputs/custom-modules -module-name ClangModuleWithOverlay -o %t
// RUN: %target-swift-frontend %s -sdk %S/Inputs -I %S/Inputs/custom-modules -I %t -typecheck
// RUN: %target-swift-frontend %s -I %t -typecheck -show-diagnostics-after-fatal -verify

// When run without the underlying SDK, we should get an error here.
import ClangModuleWithOverlay // expected-error{{cannot load underlying module for 'ClangModuleWithOverlay'}}
