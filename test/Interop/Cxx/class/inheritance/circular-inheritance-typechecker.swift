// RUN: %empty-directory(%t/index)
// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %S/Inputs -cxx-interoperability-mode=default -index-store-path %t/index
//
// Note that we specify an -index-store-path to ensure we also test importing symbolic C++ decls,
// to exercise code that handles importing unspecialized class templates.

import CircularInheritance

let voidEgg = VoidEgg()
voidEgg.dinoEgg()
voidEgg.voidEgg()

let boolEgg = BoolEgg(false)
boolEgg.dinoEgg()
boolEgg.voidEgg()
boolEgg.chickenEgg(true)

let eggEgg = EggEgg(VoidEgg())
eggEgg.dinoEgg()
eggEgg.voidEgg()
eggEgg.chickenEgg(VoidEgg())

let newEgg = NewEgg()
newEgg.dinoEgg()
newEgg.voidEgg()
newEgg.chickenEgg(555)
newEgg.newEgg()
