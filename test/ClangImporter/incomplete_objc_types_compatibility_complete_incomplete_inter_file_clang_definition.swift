// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-clang %S/Inputs/custom-modules/IncompleteTypes/incomplete-type-library-1.m -c -o %t/incomplete-type-library-1.o
// RUN: %target-clang %S/Inputs/custom-modules/IncompleteTypes/complete-types.m -c -o %t/complete-types.o

// RUN: %target-build-swift -Xfrontend -enable-upcoming-feature -Xfrontend ImportObjcForwardDeclarations -Xfrontend -enable-objc-interop -I %S/Inputs/custom-modules/IncompleteTypes %t/full_definition.swift %t/incomplete_definition.swift %t/incomplete-type-library-1.o %t/complete-types.o -Xlinker -framework -Xlinker Foundation -o %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: swift_feature_ImportObjcForwardDeclarations

//--- full_definition.swift

import CompleteTypes
import IncompleteTypeLibrary1

func getACompleteForwardDeclaredInterface() -> ForwardDeclaredInterface {
    return ForwardDeclaredInterface()
}

func takeACompleteForwardDeclaredInterface(_ param: ForwardDeclaredInterface) {
    param.doSomethingForwardDeclaredInterfacesCan()
}

//--- incomplete_definition.swift

import IncompleteTypeLibrary1

@main
class Main {
    static func main() {
        let incompleteForwardDeclaredInterface = CFunctionReturningAForwardDeclaredInterface1()!
        takeACompleteForwardDeclaredInterface(incompleteForwardDeclaredInterface)
        let completeForwardDeclaredInterface = getACompleteForwardDeclaredInterface()
        assert(type(of: incompleteForwardDeclaredInterface) == type(of: completeForwardDeclaredInterface))
    }
}
