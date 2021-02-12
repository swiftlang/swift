// RUN: %target-swift-frontend -module-name mangling -Xllvm -sil-full-demangle %s -emit-silgen | %FileCheck %s

// Note: adapted from test/SILGen/mangling.swift.

import _Differentiation

// CHECK-LABEL: sil hidden [ossa] @$s8mangling15nonescapingFunc2fnyS2fXE_tF : $@convention(thin) (@noescape @callee_guaranteed (Float) -> Float) -> () {
func nonescapingFunc(fn: (Float) -> Float) {}

// CHECK-LABEL: sil hidden [ossa] @$s8mangling8diffFunc2fnyS2fXF_tF : $@convention(thin) (@differentiable(reverse) @noescape @callee_guaranteed (Float) -> Float) -> () {
func diffFunc(fn: @differentiable(reverse) (Float) -> Float) {}

// CHECK-LABEL: sil hidden [ossa] @$s8mangling10linearFunc2fnyS2fXF_tF : $@convention(thin) (@differentiable(reverse) @noescape @callee_guaranteed (Float) -> Float) -> () {
func linearFunc(fn: @differentiable(reverse) (Float) -> Float) {}

// CHECK-LABEL: sil hidden [ossa] @$s8mangling12escapingFunc2fnS2fcS2fc_tF : $@convention(thin) (@guaranteed @callee_guaranteed (Float) -> Float) -> @owned @callee_guaranteed (Float) -> Float {
func escapingFunc(fn: @escaping (Float) -> Float) -> (Float) -> Float { fn }

// CHECK-LABEL: sil hidden [ossa] @$s8mangling16diffEscapingFunc2fnS2fXGS2fXG_tF : $@convention(thin) (@guaranteed @differentiable(reverse) @callee_guaranteed (Float) -> Float) -> @owned @differentiable(reverse) @callee_guaranteed (Float) -> Float {
func diffEscapingFunc(fn: @escaping @differentiable(reverse) (Float) -> Float) -> @differentiable(reverse) (Float) -> Float { fn }

// CHECK-LABEL: sil hidden [ossa] @$s8mangling18linearEscapingFunc2fnS2fXIS2fXI_tF : $@convention(thin) (@guaranteed @differentiable(_linear) @callee_guaranteed (Float) -> Float) -> @owned @differentiable(_linear) @callee_guaranteed (Float) -> Float {
func linearEscapingFunc(fn: @escaping @differentiable(_linear) (Float) -> Float) -> @differentiable(_linear) (Float) -> Float { fn }
