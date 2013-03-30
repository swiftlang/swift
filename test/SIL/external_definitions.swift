// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -triple x86_64-apple-darwin10 -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s -emit-sil | FileCheck %s

import ansible

func anse(a:Ansible) {}

anse(new Ansible(initWithBellsOn=new NSObject))

// -- Constructors for imported NSObject
// CHECK: func_decl constructor.allocator.1 : $(NSObject.metatype)() -> NSObject
// CHECK: func_decl constructor.initializer.1 : $(NSObject)() -> NSObject

// -- Constructors for imported Ansible
// CHECK: func_decl constructor.allocator.1 : $(Ansible.metatype)(initWithBellsOn : NSObject) -> Ansible
// CHECK: func_decl constructor.initializer.1 : $(Ansible)(initWithBellsOn : NSObject) -> Ansible

