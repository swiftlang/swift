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
        // let _ = publEnum.publEnumValue1
        // let _ = privEnum.privEnumValue1
        //
        // let _ = publEnumClass.publEnumClassValue1
        // let _ = privEnumClass.privEnumClassValue1
        //
        // let _ = publEnumAnonValue1
        // let _ = privEnumAnonValue1
        //
        // let _ = publEnumClosed.Value1
        // let _ = privEnumClosed.Value1
        //
        // let _ = publEnumOpen.Value1
        // let _ = privEnumOpen.Value1
    }

    func fcutd(_ _: publTypedef) { }
    private func fcitd(_ _: privTypedef) { }
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
    // let _ = MyClass.publEnum.publEnumValue1
    // let _ = MyClass.privEnum.privEnumValue1
    //
    // let _ = MyClass.publEnumClass.publEnumClassValue1
    // let _ = MyClass.privEnumClass.privEnumClassValue1
    //
    // let _ = MyClass.publEnumAnonValue1
    // let _ = MyClass.privEnumAnonValue1
    //
    // let _ = MyClass.publEnumClosed.Value1
    // let _ = MyClass.privEnumClosed.Value1
    //
    // let _ = MyClass.publEnumOpen.Value1
    // let _ = MyClass.privEnumOpen.Value1
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
        // let _ = publEnum.publEnumValue1
        // let _ = privEnum.privEnumValue1
        //
        // let _ = publEnumClass.publEnumClassValue1
        // let _ = privEnumClass.privEnumClassValue1
        //
        // let _ = publEnumAnonValue1
        // let _ = privEnumAnonValue1
        //
        // let _ = publEnumClosed.Value1
        // let _ = privEnumClosed.Value1
        //
        // let _ = publEnumOpen.Value1
        // let _ = privEnumOpen.Value1
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
    // let _ = MyClass.publEnum.publEnumValue1
    // let _ = MyClass.privEnum.privEnumValue1
    //
    // let _ = MyClass.publEnumClass.publEnumClassValue1
    // let _ = MyClass.privEnumClass.privEnumClassValue1
    //
    // let _ = MyClass.publEnumAnonValue1
    // let _ = MyClass.privEnumAnonValue1
    //
    // let _ = MyClass.publEnumClosed.Value1
    // let _ = MyClass.privEnumClosed.Value1
    //
    // let _ = MyClass.publEnumOpen.Value1
    // let _ = MyClass.privEnumOpen.Value1
}
