// RUN: %target-swift-frontend -sdk %S/Inputs %s -emit-silgen | %FileCheck %s

// REQUIRES: objc_interop

import ansible

var a = NSAnse(Ansible(bellsOn: NSObject()))

var anse = NSAnse

hasNoPrototype()

// CHECK-LABEL: sil @main
// -- Foreign function is referenced with C calling conv and ownership semantics
// CHECK:   [[NSANSE:%.*]] = function_ref @NSAnse : $@convention(c) (Optional<Ansible>) -> @autoreleased Optional<Ansible>
// CHECK:   [[ANSIBLE_CTOR:%.*]] = function_ref @_T0So7AnsibleC{{[_0-9a-zA-Z]*}}fC
// CHECK:   [[NSOBJECT_CTOR:%.*]] = function_ref @_T0So8NSObjectC{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@thick NSObject.Type) -> @owned NSObject
// CHECK:   [[ANSIBLE:%.*]] = apply [[ANSIBLE_CTOR]]
// CHECK:   [[NSANSE_RESULT:%.*]] = apply [[NSANSE]]([[ANSIBLE]])
// CHECK:   destroy_value [[ANSIBLE]] : $Optional<Ansible>
// -- Referencing unapplied C function goes through a thunk
// CHECK:   [[NSANSE:%.*]] = function_ref @_T0SC6NSAnseSQySo7AnsibleCGADFTO : $@convention(thin) (@owned Optional<Ansible>) -> @owned Optional<Ansible>
// -- Referencing unprototyped C function passes no parameters
// CHECK:   [[NOPROTO:%.*]] = function_ref @hasNoPrototype : $@convention(c) () -> ()
// CHECK:   apply [[NOPROTO]]()

// -- Constructors for imported Ansible
// CHECK-LABEL: sil  shared @_T0So7AnsibleC{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@in Optional<Any>, @thick Ansible.Type) -> @owned Optional<Ansible>


// -- Constructors for imported NSObject
// CHECK-LABEL: sil  shared @_T0So8NSObjectC{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@thick NSObject.Type) -> @owned NSObject

// -- Native Swift thunk for NSAnse
// CHECK: sil shared [serialized] [thunk] @_T0SC6NSAnseSQySo7AnsibleCGADFTO : $@convention(thin) (@owned Optional<Ansible>) -> @owned Optional<Ansible> {
// CHECK: bb0(%0 : $Optional<Ansible>):
// CHECK:   %1 = function_ref @NSAnse : $@convention(c) (Optional<Ansible>) -> @autoreleased Optional<Ansible>
// CHECK:   %2 = apply %1(%0) : $@convention(c) (Optional<Ansible>) -> @autoreleased Optional<Ansible>
// CHECK:   destroy_value %0 : $Optional<Ansible>
// CHECK:   return %2 : $Optional<Ansible>
// CHECK: }

// -- Constructor for imported Ansible was unused, should not be emitted.
// CHECK-NOT: sil  shared @_T0So7AnsibleC{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@thick Ansible.Type) -> @owned Ansible

