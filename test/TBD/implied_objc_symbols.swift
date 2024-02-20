// REQUIRES: VENDOR=apple
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -import-objc-header %S/Inputs/objc_class_header.h -validate-tbd-against-ir=missing %t/main.swift -disable-objc-attr-requires-foundation-module -emit-tbd -emit-tbd-path %t/main.tbd -tbd-install_name objc_classes

// RUN: %validate-json %t/main.tbd |  %FileCheck %s --check-prefix FORMAT
// RUN: %llvm-nm %t/main.tbd |  %FileCheck %s --check-prefix SYMS

// FORMAT-NOT: '_OBJC_CLASS'
// FORMAT: "objc_class"


// SYMS:  D _OBJC_CLASS_$_CApi
// SYMS:  D _OBJC_CLASS_$__TtC4test11AnotherCAPI
// SYMS:  D _OBJC_METACLASS_$_CApi
// SYMS:  D _OBJC_METACLASS_$__TtC4test11AnotherCAPI

/// Expect stand-alone metaclass for NotCAPIBase.
// SYMS:  D _OBJC_METACLASS_$__TtC4test11NotCAPIBase

// SYMS-NOT: _OBJC


//--- main.swift
import Foundation

// These declarations imply direct obj-c availability.
@objc(CApi) public class Api {}
public class AnotherCAPI : NSObject {}

public struct Empty {}
// A generic one does not, however the obj-c metaclass should still be exported.
public class NotCAPI<T> : AnotherCAPI {}
public class NotCAPIBase : NotCAPI<Empty> {}
