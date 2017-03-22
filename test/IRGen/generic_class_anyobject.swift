// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -primary-file %s -emit-ir | %FileCheck %s

// REQUIRES: CPU=i386_or_x86_64
// REQUIRES: objc_interop

func foo<T: AnyObject>(_ x: T) -> T { return x }

// CHECK-LABEL: define hidden swiftcc %objc_object* @_T023generic_class_anyobject3bars9AnyObject_psAC_pF(%objc_object*)
// CHECK:         call swiftcc %objc_object* @_T023generic_class_anyobject3foo{{[_0-9a-zA-Z]*}}F
func bar(_ x: AnyObject) -> AnyObject { return foo(x) }
