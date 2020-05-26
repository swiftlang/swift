// RUN: %target-swift-frontend -typecheck %s -I %clang-importer-sdk-path/usr/include -verify
// REQUIRES: OS=windows-msvc

import WinSDK
import WinBOOL

func CheckToBool(_: Bool) {}

// Check `BOOL` to `Bool` conversion
CheckToBool(GetBOOL())

// Check passing `BOOL` to `BOOL`
TakeBOOL(GetBOOL())

// Check discarded assignment
_ = GetBOOL()

// Check assignment to `WinSDK.WindowsBool`
let b: WindowsBool = WindowsBool(GetBOOL())

// Check assignment to `Bool`
let v: Bool = GetBOOL()

// Check conversion from boolean literal to `BOOL`
TakeBOOL(true)

// Check conversion from `Bool` to `BOOL`
let f: Bool = false
TakeBOOL(f)

