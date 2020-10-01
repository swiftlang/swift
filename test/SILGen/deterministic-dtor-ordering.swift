// RUN: %target-swift-emit-silgen -module-name horse -primary-file %S/Inputs/deterministic-dtor-ordering-other.swift -primary-file %s -module-name horse | %FileCheck %s
// RUN: %target-swift-emit-silgen -module-name horse %S/Inputs/deterministic-dtor-ordering-other.swift -primary-file %s -module-name horse | %FileCheck %s

public class Horse {}

// CHECK-LABEL: sil hidden [exact_self_class] [ossa] @$s5horse5HorseCACycfC : $@convention(method) (@thick Horse.Type) -> @owned Horse {
// CHECK-LABEL: sil hidden [ossa] @$s5horse5HorseCACycfc : $@convention(method) (@owned Horse) -> @owned Horse {
// CHECK-LABEL: sil [ossa] @$s5horse5HorseCfd : $@convention(method) (@guaranteed Horse) -> @owned Builtin.NativeObject {
// CHECK-LABEL: sil [ossa] @$s5horse5HorseCfD : $@convention(method) (@owned Horse) -> () {

