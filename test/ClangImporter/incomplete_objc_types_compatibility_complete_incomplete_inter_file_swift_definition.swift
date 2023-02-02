// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-build-swift -parse-as-library %S/Inputs/custom-modules/IncompleteTypes/complete-swift-types.swift -emit-module -emit-module-path %t/CompleteSwiftTypes.swiftmodule -emit-objc-header -emit-objc-header-path %t/CompleteSwiftTypes-Swift.h -emit-library -o %t/libCompleteSwiftTypes.dylib
// RUN: %target-clang -framework Foundation -dynamiclib %S/Inputs/custom-modules/IncompleteTypes/objc-library-forward-declaring-complete-swift-types.m -I %t -L %t -lCompleteSwiftTypes -o %t/libObjCLibraryForwardDeclaringCompleteSwiftTypes.dylib

// RUN: %target-build-swift -Xfrontend -enable-resolve-objc-forward-declarations-of-swift-types -Xfrontend -enable-objc-interop %t/full_definition.swift %t/incomplete_definition.swift -I %S/Inputs/custom-modules/IncompleteTypes -I %t -L %t -lCompleteSwiftTypes -lObjCLibraryForwardDeclaringCompleteSwiftTypes -o %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: objc_interop

//--- full_definition.swift

import CompleteSwiftTypes

func getACompleteFoo() -> Foo {
    return Foo()
}

func takeACompleteFoo(_ param: Foo) {
    param.sayHello()
}

//--- incomplete_definition.swift

import ObjCLibraryForwardDeclaringCompleteSwiftTypes

@main
class Main {
    static func main() {
        let incompleteFoo = returnAFoo()!
        takeACompleteFoo(incompleteFoo)
        let completeFoo = getACompleteFoo()
        assert(type(of: incompleteFoo) == type(of: completeFoo))
    }
}
