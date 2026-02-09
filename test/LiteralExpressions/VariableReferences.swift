// Constant globals using @section initialized with literal expressions with simple variable references
// REQUIRES: swift_feature_LiteralExpressions

// RUN: %empty-directory(%t/deps)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -dump-ast %t/client.swift -I %t/deps -enable-experimental-feature LiteralExpressions -verify | %FileCheck %s

//--- deps/foo.h
#define MACRO_INT 42

const int const_int = 42;
static const int static_const_int = 42;
static const char static_const_char = 42;
static const char static_const_char_with_long_literal = 42ull;
static const long static_const_long = 42;
static const char static_const_char_referencing_other_const = 1 + static_const_char;

static const float static_const_float = 42.0;
static const double static_const_double = 42.0;

static const char *static_const_pointer = 0;

//--- deps/module.modulemap
module Foo {
  header "foo.h"
  export *
}

//--- client.swift
import Foo
@section("mysection") let clangConstantOp1 = 1 + MACRO_INT
// CHECK-LABEL: (pattern_named type="Int32" "clangConstantOp1")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int32" location={{.*}}client.swift:{{.*}} range=[{{.*}}] value="43"

@section("mysection") let clangConstantOp2 = 2 + const_int
// CHECK-LABEL: (pattern_named type="Int32" "clangConstantOp2")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int32" location={{.*}}client.swift:{{.*}} range=[{{.*}}] value="44"

@section("mysection") let clangConstantOp4 = 4 + static_const_int
// CHECK-LABEL: (pattern_named type="Int32" "clangConstantOp4")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int32" location={{.*}}client.swift:{{.*}} range=[{{.*}}] value="46"

@section("mysection") let clangConstantOp5 = 5 + static_const_char
// CHECK-LABEL: (pattern_named type="Int8" "clangConstantOp5")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int8" location={{.*}}client.swift:{{.*}} range=[{{.*}}] value="47"

@section("mysection") let clangConstantOp6 = 6 + static_const_char_with_long_literal
// CHECK-LABEL: (pattern_named type="Int8" "clangConstantOp6")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int8" location={{.*}}client.swift:{{.*}} range=[{{.*}}] value="48"

@section("mysection") let swiftConstantOp1 = 1 + clangConstantOp1
// CHECK-LABEL: (pattern_named type="Int32" "swiftConstantOp1")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int32" location={{.*}}client.swift:{{.*}} range=[{{.*}}] value="44"
