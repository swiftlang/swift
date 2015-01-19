// RUN: %target-swift-frontend -primary-file %s -emit-ir | FileCheck %s

// REQUIRES: CPU=i386_or_x86_64
// REQUIRES: objc_interop

func foo<T: AnyObject>(x: T) -> T { return x }

// CHECK-LABEL: define hidden %objc_object* @_TF23generic_class_anyobject3barFPSs9AnyObject_PS0__(%objc_object*)
// CHECK:         call %objc_object* @_TF23generic_class_anyobject3fooUSs9AnyObject__FQ_Q_
func bar(x: AnyObject) -> AnyObject { return foo(x) }
