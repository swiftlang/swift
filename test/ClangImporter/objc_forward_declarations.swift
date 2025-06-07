// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t -I %S/Inputs/custom-modules -enable-objc-interop %s
// RUN: %target-swift-ide-test -print-module -module-to-print objc_forward_declarations -I %t -I %S/Inputs/custom-modules -enable-objc-interop -enable-upcoming-feature ImportObjcForwardDeclarations -source-filename x | %FileCheck %s

// REQUIRES: swift_feature_ImportObjcForwardDeclarations

// CHECK: class Innocuous : Confusing {

import ForwardDeclarationsHelper

public class Innocuous: Confusing {}
