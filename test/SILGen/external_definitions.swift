// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -triple x86_64-apple-darwin10 -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s -emit-silgen | FileCheck %s

import ansible

var a = NSAnse(Ansible(initWithBellsOn:NSObject()))

// -- Foreign function is referenced with C calling conv and ownership semantics
// CHECK: sil internal @top_level_code
// CHECK:   [[NSANSE:%.*]] = function_ref @NSAnse : $[cc(cdecl), thin] (x : Ansible) -> Ansible
// CHECK:   [[ANSIBLE_CTOR:%.*]] = function_ref @_TCSo7AnsibleCfMS_FT15initWithBellsOnCSo8NSObject_S_ : $[thin] ((initWithBellsOn : NSObject), Ansible.metatype) -> Ansible
// CHECK:   [[NSOBJECT_CTOR:%.*]] = function_ref @_TCSo8NSObjectCfMS_FT_S_ : $[thin] ((), NSObject.metatype) -> NSObject
// CHECK:   [[ANSIBLE:%.*]] = apply [[ANSIBLE_CTOR]]
// CHECK:   [[NSANSE_RESULT:%.*]] = apply [[NSANSE]]([[ANSIBLE]])
// CHECK:   retain_autoreleased [[NSANSE_RESULT]]
// CHECK:   release [[ANSIBLE]]

// -- Constructors for imported Ansible
// CHECK: sil clang_thunk @_TCSo7AnsibleCfMS_FT15initWithBellsOnCSo8NSObject_S_ : $[thin] ((initWithBellsOn : NSObject), Ansible.metatype) -> Ansible


// -- Constructors for imported NSObject
// CHECK: sil clang_thunk @_TCSo8NSObjectCfMS_FT_S_ : $[thin] ((), NSObject.metatype) -> NSObject
// CHECK: sil clang_thunk @_TCSo8NSObjectcfMS_FT_S_ : $[cc(method), thin] ((), NSObject) -> NSObject

// -- Constructors for imported Ansible
// CHECK: sil clang_thunk @_TCSo7AnsiblecfMS_FT15initWithBellsOnCSo8NSObject_S_ : $[cc(method), thin] ((initWithBellsOn : NSObject), Ansible) -> Ansible
// CHECK: sil clang_thunk @_TCSo7AnsibleCfMS_FT_S_ : $[thin] ((), Ansible.metatype) -> Ansible
// CHECK: sil clang_thunk @_TCSo7AnsiblecfMS_FT_S_ : $[cc(method), thin] ((), Ansible) -> Ansible

