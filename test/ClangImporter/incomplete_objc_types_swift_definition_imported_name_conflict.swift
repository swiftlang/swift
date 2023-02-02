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
let qux = Qux()

// CHECK: Hello from Qux.sayHello!
takeAConflictingTypeName(qux)
// CHECK: Hello from Qux.sayHello!
let qux2 = returnAConflictingTypeName()
// CHECK: Hello from Qux.sayHello!
qux2!.sayHello()
