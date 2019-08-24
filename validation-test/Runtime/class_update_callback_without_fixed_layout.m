// Check that when Objective-C is first to touch a Swift class, it gives the
// Swift runtime a chance to update instance size and ivar offset metadata.

// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-library -emit-module -o %t/libResilient.dylib %S/Inputs/class-layout-from-objc/Resilient.swift -Xlinker -install_name -Xlinker @executable_path/libResilient.dylib -enable-library-evolution -DSMALL

// RUN: %target-clang -c %S/Inputs/class-layout-from-objc/OneWordSuperclass.m -fmodules -fobjc-arc -o %t/OneWordSuperclass.o
// RUN: %target-build-swift -emit-library -o %t/libClasses.dylib -emit-objc-header-path %t/Classes.h -I %t -I %S/Inputs/class-layout-from-objc/ %S/Inputs/class-layout-from-objc/Classes.swift %t/OneWordSuperclass.o -Xlinker -install_name -Xlinker @executable_path/libClasses.dylib -lResilient -L %t -target %target-stable-abi-triple
// RUN: %target-clang %S/class_update_callback_with_fixed_layout.m -I %S/Inputs/class-layout-from-objc/ -I %t -fmodules -fobjc-arc -o %t/main -lResilient -lClasses -L %t
// RUN: %target-codesign %t/main %t/libResilient.dylib %t/libClasses.dylib
// RUN: %target-run %t/main NEW %t/libResilient.dylib %t/libClasses.dylib

// RUN: %target-build-swift -emit-library -emit-module -o %t/libResilient.dylib %S/Inputs/class-layout-from-objc/Resilient.swift -Xlinker -install_name -Xlinker @executable_path/libResilient.dylib -enable-library-evolution -DBIG
// RUN: %target-codesign %t/libResilient.dylib
// RUN: %target-run %t/main NEW %t/libResilient.dylib %t/libClasses.dylib

// Try again when the class itself is also resilient.
// RUN: %target-build-swift -emit-library -o %t/libClasses.dylib -emit-objc-header-path %t/Classes.h -I %S/Inputs/class-layout-from-objc/ -I %t %S/Inputs/class-layout-from-objc/Classes.swift %t/OneWordSuperclass.o -Xlinker -install_name -Xlinker @executable_path/libClasses.dylib -lResilient -L %t -target %target-stable-abi-triple
// RUN: %target-codesign %t/libClasses.dylib
// RUN: %target-run %t/main NEW %t/libResilient.dylib %t/libClasses.dylib

// RUN: %target-build-swift -emit-library -emit-module -o %t/libResilient.dylib %S/Inputs/class-layout-from-objc/Resilient.swift -Xlinker -install_name -Xlinker @executable_path/libResilient.dylib -enable-library-evolution -DSMALL
// RUN: %target-codesign %t/libResilient.dylib
// RUN: %target-run %t/main NEW %t/libResilient.dylib %t/libClasses.dylib

// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: swift_stable_abi

// The actual source code for the test is in class_update_callback_with_fixed_layout.m.
