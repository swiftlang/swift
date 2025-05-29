// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -sdk %S/Inputs %s -enable-objc-interop | %FileCheck %s

import ansible

var a = NSAnse(Ansible(bellsOn: NSObject()))

var anse = NSAnse

hasNoPrototype()

// CHECK-LABEL: sil [ossa] @main
// -- Foreign function is referenced with C calling conv and ownership semantics
// CHECK:   [[NSOBJECT_CTOR:%.*]] = function_ref @$sSo8NSObjectC{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@thick NSObject.Type) -> @owned NSObject
// CHECK:   [[ANSIBLE_CTOR:%.*]] = function_ref @$sSo7AnsibleC{{[_0-9a-zA-Z]*}}fC
// CHECK:   [[ANSIBLE:%.*]] = apply [[ANSIBLE_CTOR]]
// CHECK:   [[NSANSE:%.*]] = function_ref @NSAnse : $@convention(c) (Optional<Ansible>) -> @autoreleased Optional<Ansible>
// CHECK:   [[NSANSE_RESULT:%.*]] = apply [[NSANSE]]([[ANSIBLE]])
// CHECK:   destroy_value [[ANSIBLE]] : $Optional<Ansible>
// -- Referencing unapplied C function goes through a thunk
// CHECK:   [[NSANSE:%.*]] = function_ref @$sSo6NSAnseySo7AnsibleCSgADFTO : $@convention(thin) (@guaranteed Optional<Ansible>) -> @owned Optional<Ansible>
// -- Referencing unprototyped C function passes no parameters
// CHECK:   [[NOPROTO:%.*]] = function_ref @hasNoPrototype : $@convention(c) () -> ()
// CHECK:   apply [[NOPROTO]]()

// -- Constructors for imported NSObject
// CHECK-LABEL: sil shared [serialized] [ossa] @$sSo8NSObjectC{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@thick NSObject.Type) -> @owned NSObject

// -- Constructors for imported Ansible
// CHECK-LABEL: sil shared [serialized] [ossa] @$sSo7AnsibleC{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@in Optional<Any>, @thick Ansible.Type) -> @owned Optional<Ansible>

// -- Native Swift thunk for NSAnse
// CHECK: sil shared [serialized] [thunk] [ossa] @$sSo6NSAnseySo7AnsibleCSgADFTO : $@convention(thin) (@guaranteed Optional<Ansible>) -> @owned Optional<Ansible> {
// CHECK: bb0([[ARG0:%.*]] : @guaranteed $Optional<Ansible>):
// CHECK:   [[ARG0_COPY:%.*]] = copy_value [[ARG0]]
// CHECK:   [[FUNC:%.*]] = function_ref @NSAnse : $@convention(c) (Optional<Ansible>) -> @autoreleased Optional<Ansible>
// CHECK:   [[RESULT:%.*]] = apply [[FUNC]]([[ARG0_COPY]]) : $@convention(c) (Optional<Ansible>) -> @autoreleased Optional<Ansible>
// CHECK:   destroy_value [[ARG0_COPY]] : $Optional<Ansible>
// CHECK:   return [[RESULT]] : $Optional<Ansible>
// CHECK: }

// -- Constructor for imported Ansible was unused, should not be emitted.
// CHECK-NOT: sil {{.*}} [ossa] @$sSo7AnsibleC{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@thick Ansible.Type) -> @owned Ansible

