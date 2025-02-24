// This test checks that the private_fileid attribute (SWIFT_PRIVATE_FILEID)
// works as intended in class templates.
//
// At a high level, we are testing that, given some template class F<_> with
// instantiations F<T> and F<U>:
//
// -  non-public members of F<T> can be accessed in extensions of F<T>
//    (and same for F<U>)
//
// -  non-public members of F<T> cannot be access in extensions of F<U>
//    (and same for F<U>/F<T>)
//
// See private-fileid-typecheck.swift for tests relating to other aspects of
// this feature, such as more kinds of non-public class members, accessing
// non-public in different contexts, and variations in module and file names.
//
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs -cxx-interoperability-mode=default -module-name main %t/blessed.swift

//--- blessed.swift

import NonPublic

// These extension methods are just here to make it clear what we are doing.
extension Int32         { func read() { } }
extension Float         { func read() { } }
extension MyClass       { func read() { } }
extension MyFloatyClass { func read() { } }
extension MyClassyClass { func read() { } }

extension MyFloatyClass {
    func ext(t: Float, c: MyClass, x: MyClassyClass) {
        publMethod()
        privMethod()
        publMethodT(t).read()
        privMethodT(t).read()

        publVar.read()
        privVar.read()
        publVarT.read()
        privVarT.read()

        let _: publTypedef
        let _: privTypedef
        let _: publTypedefT = t
        let _: privTypedefT = t

        let _: MyFloatyClass.publTypedef
        let _: MyFloatyClass.privTypedef
        let _: MyFloatyClass.publTypedefT = t
        let _: MyFloatyClass.privTypedefT = t

        x.publMethod()
        x.privMethod()          // expected-error {{'privMethod' is inaccessible due to 'private' protection level}}
        x.publMethodT(c).read()
        x.privMethodT(c).read() // expected-error {{'privMethodT' is inaccessible due to 'private' protection level}}

        x.publVar.read()
        x.privVar.read()  // expected-error {{'privVar' is inaccessible due to 'private' protection level}}
        x.publVarT.read()
        x.privVarT.read() // expected-error {{'privVarT' is inaccessible due to 'private' protection level}}

        let _: MyClassyClass.publTypedef
        let _: MyClassyClass.privTypedef  // expected-error {{'privTypedef' is inaccessible due to 'private' protection level}}
        let _: MyClassyClass.publTypedefT
        let _: MyClassyClass.privTypedefT // expected-error {{'privTypedefT' is inaccessible due to 'private' protection level}}
    }
}

extension MyClassyClass {
    func ext(t: MyClass, c: Float, x: MyFloatyClass) {
        publMethod()
        privMethod()
        publMethodT(t).read()
        privMethodT(t).read()

        publVar.read()
        privVar.read()
        publVarT.read()
        privVarT.read()

        let _: publTypedef
        let _: privTypedef
        let _: publTypedefT = t
        let _: privTypedefT = t

        let _: MyClassyClass.publTypedef
        let _: MyClassyClass.privTypedef
        let _: MyClassyClass.publTypedefT = t
        let _: MyClassyClass.privTypedefT = t

        x.publMethod()
        x.privMethod()          // expected-error {{'privMethod' is inaccessible due to 'private' protection level}}
        x.publMethodT(c).read()
        x.privMethodT(c).read() // expected-error {{'privMethodT' is inaccessible due to 'private' protection level}}

        x.publVar.read()
        x.privVar.read()  // expected-error {{'privVar' is inaccessible due to 'private' protection level}}
        x.publVarT.read()
        x.privVarT.read() // expected-error {{'privVarT' is inaccessible due to 'private' protection level}}

        let _: MyFloatyClass.publTypedef
        let _: MyFloatyClass.privTypedef  // expected-error {{'privTypedef' is inaccessible due to 'private' protection level}}
        let _: MyFloatyClass.publTypedefT
        let _: MyFloatyClass.privTypedefT // expected-error {{'privTypedefT' is inaccessible due to 'private' protection level}}
    }
}
