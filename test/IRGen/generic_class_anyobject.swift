// RUN: %target-swift-frontend -primary-file %s -emit-ir | %FileCheck %s

// REQUIRES: CPU=i386_or_x86_64
// REQUIRES: objc_interop

func foo<T: AnyObject>(_ x: T) -> T { return x }

// CHECK-LABEL: define hidden %objc_object* @_TF23generic_class_anyobject3barFPs9AnyObject_PS0__(%objc_object*)
// CHECK:         call %objc_object* @_TF23generic_class_anyobject3foo
func bar(_ x: AnyObject) -> AnyObject { return foo(x) }
