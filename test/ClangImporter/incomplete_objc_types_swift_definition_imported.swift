// RUN: %empty-directory(%t)
// RUN: %target-build-swift -parse-as-library %S/Inputs/custom-modules/IncompleteTypes/complete-swift-types.swift -emit-module -emit-module-path %t/CompleteSwiftTypes.swiftmodule -emit-objc-header -emit-objc-header-path %t/CompleteSwiftTypes-Swift.h -emit-library -o %t/libCompleteSwiftTypes.dylib
// RUN: %target-clang -framework Foundation -dynamiclib %S/Inputs/custom-modules/IncompleteTypes/objc-library-forward-declaring-complete-swift-types.m -I %t -L %t -lCompleteSwiftTypes -o %t/libObjCLibraryForwardDeclaringCompleteSwiftTypes.dylib
// RUN: %target-build-swift -Xfrontend -enable-resolve-objc-forward-declarations-of-swift-types -Xfrontend -enable-objc-interop %s -I %S/Inputs/custom-modules/IncompleteTypes -I %t -L %t -lCompleteSwiftTypes -lObjCLibraryForwardDeclaringCompleteSwiftTypes -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: executable_test

import CompleteSwiftTypes
import ObjCLibraryForwardDeclaringCompleteSwiftTypes

// Swift initializers
let foo = Foo()
let bar = Bar()
let corge = Corge()

// CHECK: Hello from Foo.sayHello!
foo.sayHello()
// CHECK: Hello from Bar.sayHello!
bar.sayHello()
// CHECK: Hello from Corge.sayHello!
corge.sayHello()

// Imported from Objective-C
// CHECK: Hello from Foo.sayHello!
takeAFoo(foo)
// CHECK: Hello from Foo.sayHello!
let foo2 = returnAFoo()
// CHECK: Hello from Foo.sayHello!
foo2!.sayHello()

// CHECK: Hello from Bar.sayHello!
takeABaz(bar)
// CHECK: Hello from Bar.sayHello!
let bar2 = returnABaz()
// CHECK: Hello from Bar.sayHello!
bar2!.sayHello()

// CHECK: Hello from Corge.sayHello!
takeASubscript(corge)
// CHECK: Hello from Corge.sayHello!
let corge2 = returnASubscript()
// CHECK: Hello from Corge.sayHello!
corge2!.sayHello()
