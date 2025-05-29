// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %S/Inputs -cxx-interoperability-mode=default

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
