// RUN: %empty-directory(%t)

// Check that verification won't reject a valid interface:
// RUN: %target-build-swift -emit-library -enable-library-evolution -emit-module-interface -emit-module -swift-version 5 -o %t/MyModule.o -verify-emitted-module-interface -module-name MyModule %s

// Check that verification will reject an invalid interface:
// RUN: not %target-build-swift -emit-library -enable-library-evolution -emit-module-interface -emit-module -swift-version 5 -o %t/MyModule.o -verify-emitted-module-interface -module-name MyModule -Xfrontend -debug-emit-invalid-swiftinterface-syntax %s 2>&1 | %FileCheck %s

// ...but not if verification is off.
// RUN: %target-build-swift -emit-library -enable-library-evolution -emit-module-interface -emit-module -swift-version 5 -o %t/MyModule.o -no-verify-emitted-module-interface -module-name MyModule -Xfrontend -debug-emit-invalid-swiftinterface-syntax %s

public struct MyStruct {}

// CHECK: MyModule.swiftinterface:{{[0-9]+}}:{{[0-9]+}}: error: use of unknown directive '#__debug_emit_invalid_swiftinterface_syntax__'
// CHECK: MyModule.swiftinterface:{{[0-9]+}}:{{[0-9]+}}: error: failed to verify module interface of 'MyModule'; it may have been damaged or it may have triggered a bug in the Swift compiler when it was produced
