// RUN: %empty-directory(%t)
// RUN: %target-build-swift -parse-as-library %S/Inputs/custom-modules/IncompleteTypes/complete-swift-types.swift -emit-module -emit-module-path %t/CompleteSwiftTypes.swiftmodule -emit-objc-header -emit-objc-header-path %t/CompleteSwiftTypes-Swift.h -emit-library -o %t/libCompleteSwiftTypes.dylib -Xlinker -install_name -Xlinker @executable_path/libCompleteSwiftTypes.dylib
// RUN: %target-clang -framework Foundation -dynamiclib %S/Inputs/custom-modules/IncompleteTypes/objc-library-forward-declaring-complete-swift-types.m -I %t -L %t -lCompleteSwiftTypes -install_name @executable_path/libObjCLibraryForwardDeclaringCompleteSwiftTypes.dylib -o %t/libObjCLibraryForwardDeclaringCompleteSwiftTypes.dylib
// RUN: %target-build-swift -Xfrontend -enable-upcoming-feature -Xfrontend ImportObjcForwardDeclarations -Xfrontend -enable-objc-interop %s -I %S/Inputs/custom-modules/IncompleteTypes -I %t -L %t -lCompleteSwiftTypes -lObjCLibraryForwardDeclaringCompleteSwiftTypes -o %t/a.out
// RUN: %target-run %t/a.out %t/libCompleteSwiftTypes.dylib %t/libObjCLibraryForwardDeclaringCompleteSwiftTypes.dylib

// REQUIRES: objc_interop
// REQUIRES: executable_test
// REQUIRES: swift_feature_ImportObjcForwardDeclarations

import CompleteSwiftTypes
import ObjCLibraryForwardDeclaringCompleteSwiftTypes

let placeholderInstanceForNativeObjCInterfaceShadowedProtocol = returnANativeObjCClassShadowedProtocol()
