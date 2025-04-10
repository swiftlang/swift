// This test checks that the private_fileid attribute (SWIFT_PRIVATE_FILEID)
// works as intended, for all supported non-public C++ class members, and only
// in the contexts where they should be accessible (i.e., the files blessed by
// the SWIFT_PRIVATE_FILEID annotation).
//
// The private_fileid mechanism relies on fileIDs, so we need some control over
// file names:
//
// RUN: split-file %s %t
//
// The imported C++ struct is annotated with "main/blessed.swift", so we need to
// use "blessed.swift" as the file name and "main" as the module name.
//
// To minimize some boilerplate, we reuse the same header file but tweak whether
// we are importing a class (default) or a struct, and whether its non-public
// members are private (default) or protected. The result should be the same
// no matter the configuration.
//
// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs -cxx-interoperability-mode=default -module-name main %t/blessed.swift
// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs -cxx-interoperability-mode=default -module-name main %t/blessed.swift -Xcc -DTEST_CLASS=struct
// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs -cxx-interoperability-mode=default -module-name main %t/blessed.swift -Xcc -DTEST_PRIVATE=protected
// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs -cxx-interoperability-mode=default -module-name main %t/blessed.swift -Xcc -DTEST_CLASS=struct -Xcc -DTEST_PRIVATE=protected
//
// This test also includes a "cursed.swift", which expects to not have access to
// non-public members:
//
// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs -cxx-interoperability-mode=default -module-name main %t/cursed.swift
// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs -cxx-interoperability-mode=default -module-name main %t/cursed.swift -Xcc -DTEST_CLASS=struct
// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs -cxx-interoperability-mode=default -module-name main %t/cursed.swift -Xcc -DTEST_PRIVATE=protected
// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs -cxx-interoperability-mode=default -module-name main %t/cursed.swift -Xcc -DTEST_CLASS=struct -Xcc -DTEST_PRIVATE=protected
//
// To check that fileID is agnostic about directory structure within a module,
// we move blessed.swift into a subdirectory (but keep its filename).
//
// RUN: mkdir -p %t/subdir/subsubdir
// RUN: mv %t/blessed.swift %t/subdir/subsubdir/blessed.swift
// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs -cxx-interoperability-mode=default -module-name main %t/subdir/subsubdir/blessed.swift
// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs -cxx-interoperability-mode=default -module-name main %t/subdir/subsubdir/blessed.swift -Xcc -DTEST_CLASS=struct
// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs -cxx-interoperability-mode=default -module-name main %t/subdir/subsubdir/blessed.swift -Xcc -DTEST_PRIVATE=protected
// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs -cxx-interoperability-mode=default -module-name main %t/subdir/subsubdir/blessed.swift -Xcc -DTEST_CLASS=struct -Xcc -DTEST_PRIVATE=protected
//
// To check that fileID is sensitive to module names, rename cursed.swift to
// "blessed.swift", but typecheck in a module not called "main".
//
// RUN: mv %t/cursed.swift %t/blessed.swift
// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs -cxx-interoperability-mode=default -module-name brain %t/blessed.swift
// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs -cxx-interoperability-mode=default -module-name brain %t/blessed.swift -Xcc -DTEST_CLASS=struct
// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs -cxx-interoperability-mode=default -module-name brain %t/blessed.swift -Xcc -DTEST_PRIVATE=protected
// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs -cxx-interoperability-mode=default -module-name brain %t/blessed.swift -Xcc -DTEST_CLASS=struct -Xcc -DTEST_PRIVATE=protected

//--- blessed.swift

import NonPublic

// These extension methods are just here to make it clear what we are doing to
// each Int32-typed member.
extension Int32 {
    func read() { }
    mutating func write() { }
}

extension MyClass {
    mutating func ext() {
        publMethod()
        privMethod()

        publMutatingMethod()
        privMutatingMethod()

        publVar.read()
        privVar.read()

        publVar.write()
        privVar.write()

        MyClass.publStaticFunc()
        MyClass.privStaticFunc()

        MyClass.publStaticVar.read()
        MyClass.privStaticVar.read()

        MyClass.publStaticVar.write()
        MyClass.privStaticVar.write()

        let _: publTypedef
        let _: privTypedef

        let _: publStruct
        let _: privStruct

        let _: publEnum
        let _: privEnum

        let _: publEnumClass
        let _: privEnumClass

        let _: publEnumClosed
        let _: privEnumClosed

        let _: publEnumOpen
        let _: privEnumOpen

        let _: publEnumFlag
        let _: privEnumFlag

        // TODO: Enum variants are not being correctly imported. Test the following when that is fixed:
        // let _ = variantPublEnum
        // let _ = variantPrivEnum
        //
        // let _ = publEnumAnonVariant
        // let _ = privEnumAnonVariant

        let _ = publEnumClass.variantPublEnumClass
        let _ = privEnumClass.variantPrivEnumClass

        let _ = publEnumClosed.variantPublEnumClosed
        let _ = privEnumClosed.variantPrivEnumClosed

        let _ = publEnumOpen.variantPublEnumOpen
        let _ = privEnumOpen.variantPrivEnumOpen
    }

    // Make sure these types are usable in type signatures too
    func publTypedefFunc(_ _: publTypedef) { }
    private func privTypedefFunc(_ _: privTypedef) { }

    func publStructFunc(_ _: publStruct) { }
    private func privStructFunc(_ _: privStruct) { }

    func publEnumFunc(_ _: publEnum) { }
    private func privEnumFunc(_ _: privEnum) { }

    func publEnumClassFunc(_ _: publEnumClass) { }
    private func privEnumClassFunc(_ _: privEnumClass) { }

    func publEnumClosedFunc(_ _: publEnumClosed) { }
    private func privEnumClosedFunc(_ _: privEnumClosed) { }

    func publEnumOpenFunc(_ _: publEnumOpen) { }
    private func privEnumOpenFunc(_ _: privEnumOpen) { }

    func publEnumFlagFunc(_ _: publEnumFlag) { }
    private func privEnumFlagFunc(_ _: privEnumFlag) { }
}

func notExt(_ c: inout MyClass) {
    c.publMethod()
    c.privMethod() // expected-error {{'privMethod' is inaccessible due to 'private' protection level}}

    c.publMutatingMethod()
    c.privMutatingMethod() // expected-error {{'privMutatingMethod' is inaccessible due to 'private' protection level}}

    c.publVar.read()
    c.privVar.read() // expected-error {{'privVar' is inaccessible due to 'private' protection level}}

    MyClass.publStaticFunc()
    MyClass.privStaticFunc() // expected-error {{'privStaticFunc' is inaccessible due to 'private' protection level}}

    MyClass.publStaticVar.read()
    MyClass.privStaticVar.read() // expected-error {{'privStaticVar' is inaccessible due to 'private' protection level}}

    let _: MyClass.publTypedef
    let _: MyClass.privTypedef // expected-error {{'privTypedef' is inaccessible due to 'private' protection level}}

    let _: MyClass.publStruct
    let _: MyClass.privStruct // expected-error {{'privStruct' is inaccessible due to 'private' protection level}}

    let _: MyClass.publEnum
    let _: MyClass.privEnum // expected-error {{'privEnum' is inaccessible due to 'private' protection level}}

    let _: MyClass.publEnumClass
    let _: MyClass.privEnumClass // expected-error {{'privEnumClass' is inaccessible due to 'private' protection level}}

    let _: MyClass.publEnumClosed
    let _: MyClass.privEnumClosed // expected-error {{'privEnumClosed' is inaccessible due to 'private' protection level}}

    let _: MyClass.publEnumOpen
    let _: MyClass.privEnumOpen // expected-error {{'privEnumOpen' is inaccessible due to 'private' protection level}}

    let _: MyClass.publEnumFlag
    let _: MyClass.privEnumFlag // expected-error {{'privEnumFlag' is inaccessible due to 'private' protection level}}

    // TODO: Enum variants are not being correctly imported. Test the following when that is fixed:
    //
    // let _ = MyClass.variantPublEnum
    // let _ = MyClass.variantPrivEnum // TODO-error {{'variantPrivEnum' is inaccessible due to 'private' protection level}}
    //
    // let _ = MyClass.publEnumAnonVariant
    // let _ = MyClass.privEnumAnonVariant // TODO-error {{'privEnumAnonVariant' is inaccessible due to 'private' protection level}}

    let _ = MyClass.publEnumClass.variantPublEnumClass
    let _ = MyClass.privEnumClass.variantPrivEnumClass // expected-error {{'privEnumClass' is inaccessible due to 'private' protection level}}

    let _ = MyClass.publEnumClosed.variantPublEnumClosed
    let _ = MyClass.privEnumClosed.variantPrivEnumClosed // expected-error {{'privEnumClosed' is inaccessible due to 'private' protection level}}

    let _ = MyClass.publEnumOpen.variantPublEnumOpen
    let _ = MyClass.privEnumOpen.variantPrivEnumOpen // expected-error {{'privEnumOpen' is inaccessible due to 'private' protection level}}
}

//--- cursed.swift

import NonPublic

// These extension methods are just here to make it clear what we are doing to
// each Int32-typed member.
extension Int32 {
    func read() { }
    mutating func write() { }
}

extension MyClass {
    mutating func ext() {
        publMethod()
        privMethod() // expected-error {{'privMethod' is inaccessible due to 'private' protection level}}

        publMutatingMethod()
        privMutatingMethod() // expected-error {{'privMutatingMethod' is inaccessible due to 'private' protection level}}

        publVar.read()
        privVar.read() // expected-error {{'privVar' is inaccessible due to 'private' protection level}}

        MyClass.publStaticFunc()
        MyClass.privStaticFunc() // expected-error {{'privStaticFunc' is inaccessible due to 'private' protection level}}

        MyClass.publStaticVar.read()
        MyClass.privStaticVar.read() // expected-error {{'privStaticVar' is inaccessible due to 'private' protection level}}

        let _: publTypedef
        let _: privTypedef // expected-error {{'privTypedef' is inaccessible due to 'private' protection level}}

        let _: publStruct
        let _: privStruct // expected-error {{'privStruct' is inaccessible due to 'private' protection level}}

        let _: publEnum
        let _: privEnum // expected-error {{'privEnum' is inaccessible due to 'private' protection level}}

        let _: publEnumClass
        let _: privEnumClass // expected-error {{'privEnumClass' is inaccessible due to 'private' protection level}}

        let _: publEnumClosed
        let _: privEnumClosed // expected-error {{'privEnumClosed' is inaccessible due to 'private' protection level}}

        let _: publEnumOpen
        let _: privEnumOpen // expected-error {{'privEnumOpen' is inaccessible due to 'private' protection level}}

        let _: publEnumFlag
        let _: privEnumFlag // expected-error {{'privEnumFlag' is inaccessible due to 'private' protection level}}

        // TODO: Enum variants are not being correctly imported. Test the following when that is fixed:
        // let _ = variantPublEnum
        // let _ = variantPrivEnum // TODO-error {{'variantPrivEnum' is inaccessible due to 'private' protection level}}
        //
        // let _ = publEnumAnonVariant
        // let _ = privEnumAnonVariant // TODO-error {{'privEnumAnonVariant' is inaccessible due to 'private' protection level}}

        let _ = publEnumClass.variantPublEnumClass
        let _ = privEnumClass.variantPrivEnumClass// expected-error {{'privEnumClass' is inaccessible due to 'private' protection level}}

        let _ = publEnumClosed.variantPublEnumClosed
        let _ = privEnumClosed.variantPrivEnumClosed // expected-error {{'privEnumClosed' is inaccessible due to 'private' protection level}}

        let _ = publEnumOpen.variantPublEnumOpen
        let _ = privEnumOpen.variantPrivEnumOpen // expected-error {{'privEnumOpen' is inaccessible due to 'private' protection level}}
    }
}

func notExt(_ c: inout MyClass) {
    c.publMethod()
    c.privMethod() // expected-error {{'privMethod' is inaccessible due to 'private' protection level}}

    c.publMutatingMethod()
    c.privMutatingMethod() // expected-error {{'privMutatingMethod' is inaccessible due to 'private' protection level}}

    c.publVar.read()
    c.privVar.read() // expected-error {{'privVar' is inaccessible due to 'private' protection level}}

    MyClass.publStaticFunc()
    MyClass.privStaticFunc() // expected-error {{'privStaticFunc' is inaccessible due to 'private' protection level}}

    MyClass.publStaticVar.read()
    MyClass.privStaticVar.read() // expected-error {{'privStaticVar' is inaccessible due to 'private' protection level}}

    let _: MyClass.publTypedef
    let _: MyClass.privTypedef // expected-error {{'privTypedef' is inaccessible due to 'private' protection level}}

    let _: MyClass.publStruct
    let _: MyClass.privStruct // expected-error {{'privStruct' is inaccessible due to 'private' protection level}}

    let _: MyClass.publEnum
    let _: MyClass.privEnum // expected-error {{'privEnum' is inaccessible due to 'private' protection level}}

    let _: MyClass.publEnumClass
    let _: MyClass.privEnumClass // expected-error {{'privEnumClass' is inaccessible due to 'private' protection level}}

    let _: MyClass.publEnumClosed
    let _: MyClass.privEnumClosed // expected-error {{'privEnumClosed' is inaccessible due to 'private' protection level}}

    let _: MyClass.publEnumOpen
    let _: MyClass.privEnumOpen // expected-error {{'privEnumOpen' is inaccessible due to 'private' protection level}}

    let _: MyClass.publEnumFlag
    let _: MyClass.privEnumFlag // expected-error {{'privEnumFlag' is inaccessible due to 'private' protection level}}

    // TODO: Enum variants are not being correctly imported. Test the following when that is fixed:
    // let _ = MyClass.variantPublEnum
    // let _ = MyClass.variantPrivEnum // TODO-error {{'variantPrivEnum' is inaccessible due to 'private' protection level}}
    //
    // let _ = MyClass.publEnumAnonVariant
    // let _ = MyClass.privEnumAnonVariant // TODO-error {{'privEnumAnonVariant' is inaccessible due to 'private' protection level}}

    let _ = MyClass.publEnumClass.variantPublEnumClass
    let _ = MyClass.privEnumClass.variantPrivEnumClass// expected-error {{'privEnumClass' is inaccessible due to 'private' protection level}}

    let _ = MyClass.publEnumClosed.variantPublEnumClosed
    let _ = MyClass.privEnumClosed.variantPrivEnumClosed // expected-error {{'privEnumClosed' is inaccessible due to 'private' protection level}}

    let _ = MyClass.publEnumOpen.variantPublEnumOpen
    let _ = MyClass.privEnumOpen.variantPrivEnumOpen // expected-error {{'privEnumOpen' is inaccessible due to 'private' protection level}}
}
