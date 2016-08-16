// RUN: %target-swift-frontend -sdk %S/Inputs %s -emit-silgen | %FileCheck %s

// REQUIRES: objc_interop

import ansible

var a = NSAnse(Ansible(bellsOn: NSObject()))

var anse = NSAnse

hasNoPrototype()

// CHECK-LABEL: sil @main
// -- Foreign function is referenced with C calling conv and ownership semantics
// CHECK:   [[NSANSE:%.*]] = function_ref @NSAnse : $@convention(c) (ImplicitlyUnwrappedOptional<Ansible>) -> @autoreleased ImplicitlyUnwrappedOptional<Ansible>
// CHECK:   [[ANSIBLE_CTOR:%.*]] = function_ref @_TFCSo7AnsibleC
// CHECK:   [[NSOBJECT_CTOR:%.*]] = function_ref @_TFCSo8NSObjectC{{.*}} : $@convention(method) (@thick NSObject.Type) -> @owned NSObject
// CHECK:   [[ANSIBLE:%.*]] = apply [[ANSIBLE_CTOR]]
// CHECK:   [[NSANSE_RESULT:%.*]] = apply [[NSANSE]]([[ANSIBLE]])
// CHECK:   release_value [[ANSIBLE]] : $ImplicitlyUnwrappedOptional<Ansible>
// -- Referencing unapplied C function goes through a thunk
// CHECK:   [[NSANSE:%.*]] = function_ref @_TTOFSC6NSAnseFGSQCSo7Ansible_GSQS__ : $@convention(thin) (@owned ImplicitlyUnwrappedOptional<Ansible>) -> @owned ImplicitlyUnwrappedOptional<Ansible>
// -- Referencing unprototyped C function passes no parameters
// CHECK:   [[NOPROTO:%.*]] = function_ref @hasNoPrototype : $@convention(c) () -> ()
// CHECK:   apply [[NOPROTO]]()

// -- Constructors for imported Ansible
// CHECK-LABEL: sil  shared @_TFCSo7AnsibleC{{.*}} : $@convention(method) (@in ImplicitlyUnwrappedOptional<Any>, @thick Ansible.Type) -> @owned ImplicitlyUnwrappedOptional<Ansible>


// -- Constructors for imported NSObject
// CHECK-LABEL: sil  shared @_TFCSo8NSObjectC{{.*}} : $@convention(method) (@thick NSObject.Type) -> @owned NSObject

// -- Native Swift thunk for NSAnse
// CHECK: sil shared [fragile] [thunk] @_TTOFSC6NSAnseFGSQCSo7Ansible_GSQS__ : $@convention(thin) (@owned ImplicitlyUnwrappedOptional<Ansible>) -> @owned ImplicitlyUnwrappedOptional<Ansible> {
// CHECK: bb0(%0 : $ImplicitlyUnwrappedOptional<Ansible>):
// CHECK:   %1 = function_ref @NSAnse : $@convention(c) (ImplicitlyUnwrappedOptional<Ansible>) -> @autoreleased ImplicitlyUnwrappedOptional<Ansible>
// CHECK:   %2 = apply %1(%0) : $@convention(c) (ImplicitlyUnwrappedOptional<Ansible>) -> @autoreleased ImplicitlyUnwrappedOptional<Ansible>
// CHECK:   release_value %0 : $ImplicitlyUnwrappedOptional<Ansible>
// CHECK:   return %2 : $ImplicitlyUnwrappedOptional<Ansible>
// CHECK: }

// -- Constructor for imported Ansible was unused, should not be emitted.
// CHECK-NOT: sil  shared @_TFCSo7AnsibleC{{.*}} : $@convention(method) (@thick Ansible.Type) -> @owned Ansible

