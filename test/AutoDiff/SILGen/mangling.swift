// RUN: %target-swift-frontend -module-name mangling -Xllvm -sil-full-demangle %s -emit-silgen | %FileCheck %s

// Note: adapted from test/SILGen/mangling.swift.

import _Differentiation

// CHECK-LABEL: sil hidden [ossa] @$s8mangling15nonescapingFunc2fnyS2fXE_tF : 
func nonescapingFunc(fn: (Float) -> Float) {}

// CHECK-LABEL: sil hidden [ossa] @$s8mangling8diffFunc2fnyS2fYjrXE_tF : 
func diffFunc(fn: @differentiable(reverse) (Float) -> Float) {}

// CHECK-LABEL: sil hidden [ossa] @$s8mangling10linearFunc2fnyS2fYjlXE_tF : 
func linearFunc(fn: @differentiable(_linear) (Float) -> Float) {}

// CHECK-LABEL: sil hidden [ossa] @$s8mangling12escapingFunc2fnS2fcS2fc_tF : 
func escapingFunc(fn: @escaping (Float) -> Float) -> (Float) -> Float { fn }

// CHECK-LABEL: sil hidden [ossa] @$s8mangling16diffEscapingFunc2fnS2fYjrcS2fYjrc_tF : 
func diffEscapingFunc(fn: @escaping @differentiable(reverse) (Float) -> Float) -> @differentiable(reverse) (Float) -> Float { fn }

// CHECK-LABEL: sil hidden [ossa] @$s8mangling18linearEscapingFunc2fnS2fYjlcS2fYjlc_tF : 
func linearEscapingFunc(fn: @escaping @differentiable(_linear) (Float) -> Float) -> @differentiable(_linear) (Float) -> Float { fn }
