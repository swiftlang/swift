// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %t/src/main.swift \
// RUN:   -import-bridging-header %t/src/test.h \
// RUN:   -module-name main -emit-sil | %FileCheck %s

// Regression: previously, importing a `static const` whose declared C type
// is a `swift_wrapper(struct)` typedef'd over an enum that itself imports
// as a wrapper struct (rather than a primitive integer) tripped an
// assertion in `BuiltinLiteralExpr::setBuiltinInitializer`. The literal
// synthesizer in `SwiftDeclSynthesizer::createConstant` looked up
// `ExpressibleByBuiltinIntegerLiteral` on the resolved literal type
// (the inner wrapper struct), which has no such conformance. The witness
// was empty and the assertion fired.
//
// Fix: bail out of the inlined-literal synthesis path when the witness
// is empty; the variable is then imported as an external declaration that
// resolves to the C global at link time.

//--- test.h
typedef enum { _MyKind_Z = 0 } _MyKind;
typedef _MyKind MyKind __attribute__((swift_wrapper(struct)));
static const MyKind MyKind_None __attribute__((swift_name("none"))) = _MyKind_Z;

//--- main.swift
public func access() -> MyKind {
    return MyKind.none
}

// The constant must be imported as an external sil_global rather than
// synthesized as an inlined literal getter.
// CHECK: sil_global public_external [let] @MyKind_None : $MyKind

// CHECK-LABEL: sil {{.*}}@$s4main6access{{.*}}() -> MyKind
// CHECK: global_addr @MyKind_None : $*MyKind
