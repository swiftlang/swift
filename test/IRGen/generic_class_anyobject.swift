// RUN: %swift -target x86_64-apple-macosx10.9 %s -emit-ir | FileCheck %s

func foo<T: AnyObject>(x: T) -> T { return x }

// CHECK-LABEL: define %objc_object* @_TF23generic_class_anyobject3barFPSs9AnyObject_PS0__(%objc_object*)
// CHECK:         call %objc_object* @_TF23generic_class_anyobject3fooUSs9AnyObject__FQ_Q_
func bar(x: AnyObject) -> AnyObject { return foo(x) }
