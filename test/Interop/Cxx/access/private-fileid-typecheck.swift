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
        pubMethod()
        privMethod()

        pubMutatingMethod()
        privMutatingMethod()

        pubVar.read()
        privVar.read()

        pubVar.write()
        privVar.write()

        MyClass.pubStaticFunc()
        MyClass.privStaticFunc()

        MyClass.pubStaticVar.read()
        MyClass.privStaticVar.read()

        MyClass.pubStaticVar.write()
        MyClass.privStaticVar.write()

        let _: pubEnum
        let _: privEnum

        // TODO: Enum variants are not being correctly imported. Test the following when that is fixed:
        // let _ = pubEnum.pubEnumCase
        // let _ = privEnum.privEnumCase

        let _: pubTypedef
        let _: privTypedef

        let _: pubStruct
        let _: privStruct
    }

    func fcutd(_ _: pubTypedef) { }
    private func fcitd(_ _: privTypedef) { }
}

func notExt(_ c: inout MyClass) {
    c.pubMethod()
    c.privMethod() // expected-error {{'privMethod' is inaccessible due to 'private' protection level}}

    c.pubMutatingMethod()
    c.privMutatingMethod() // expected-error {{'privMutatingMethod' is inaccessible due to 'private' protection level}}

    c.pubVar.read()
    c.privVar.read() // expected-error {{'privVar' is inaccessible due to 'private' protection level}}

    MyClass.pubStaticFunc()
    MyClass.privStaticFunc() // expected-error {{'privStaticFunc' is inaccessible due to 'private' protection level}}

    MyClass.pubStaticVar.read()
    MyClass.privStaticVar.read() // expected-error {{'privStaticVar' is inaccessible due to 'private' protection level}}

    let _: MyClass.pubEnum
    let _: MyClass.privEnum // expected-error {{'privEnum' is inaccessible due to 'private' protection level}}

    // TODO: Enum variants are not being correctly imported. Test the following when that is fixed:
    // let _ = MyClass.pubEnum.pubEnumCase
    // let _ = MyClass.privEnum.privEnumCase

    let _: MyClass.pubTypedef
    let _: MyClass.privTypedef // expected-error {{'privTypedef' is inaccessible due to 'private' protection level}}

    let _: MyClass.pubStruct
    let _: MyClass.privStruct // expected-error {{'privStruct' is inaccessible due to 'private' protection level}}
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
        pubMethod()

        privMethod() // expected-error {{'privMethod' is inaccessible due to 'private' protection level}}

        pubMutatingMethod()
        privMutatingMethod() // expected-error {{'privMutatingMethod' is inaccessible due to 'private' protection level}}

        pubVar.read()
        privVar.read() // expected-error {{'privVar' is inaccessible due to 'private' protection level}}

        MyClass.pubStaticFunc()
        MyClass.privStaticFunc() // expected-error {{'privStaticFunc' is inaccessible due to 'private' protection level}}

        MyClass.pubStaticVar.read()
        MyClass.privStaticVar.read() // expected-error {{'privStaticVar' is inaccessible due to 'private' protection level}}

        let _: MyClass.pubEnum
        let _: MyClass.privEnum // expected-error {{'privEnum' is inaccessible due to 'private' protection level}}

        // TODO: Enum variants are not being correctly imported. Test the following when that is fixed:
        // let _ = MyClass.pubEnum.pubEnumCase
        // let _ = MyClass.privEnum.privEnumCase

        let _: MyClass.pubTypedef
        let _: MyClass.privTypedef // expected-error {{'privTypedef' is inaccessible due to 'private' protection level}}

        let _: MyClass.pubStruct
        let _: MyClass.privStruct // expected-error {{'privStruct' is inaccessible due to 'private' protection level}}
    }
}

func notExt(_ c: inout MyClass) {
    c.pubMethod()
    c.privMethod() // expected-error {{'privMethod' is inaccessible due to 'private' protection level}}

    c.pubMutatingMethod()
    c.privMutatingMethod() // expected-error {{'privMutatingMethod' is inaccessible due to 'private' protection level}}

    c.pubVar.read()
    c.privVar.read() // expected-error {{'privVar' is inaccessible due to 'private' protection level}}

    MyClass.pubStaticFunc()
    MyClass.privStaticFunc() // expected-error {{'privStaticFunc' is inaccessible due to 'private' protection level}}

    MyClass.pubStaticVar.read()
    MyClass.privStaticVar.read() // expected-error {{'privStaticVar' is inaccessible due to 'private' protection level}}

    let _: MyClass.pubEnum
    let _: MyClass.privEnum // expected-error {{'privEnum' is inaccessible due to 'private' protection level}}

    // TODO: Enum variants are not being correctly imported. Test the following when that is fixed:
    // let _ = MyClass.pubEnum.pubEnumCase
    // let _ = MyClass.privEnum.privEnumCase

    let _: MyClass.pubTypedef
    let _: MyClass.privTypedef // expected-error {{'privTypedef' is inaccessible due to 'private' protection level}}

    let _: MyClass.pubStruct
    let _: MyClass.privStruct // expected-error {{'privStruct' is inaccessible due to 'private' protection level}}
}
