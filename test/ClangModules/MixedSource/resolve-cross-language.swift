// RUN: rm -rf %t
// RUN: mkdir -p %t

// RUN: %swiftc_driver -emit-module -emit-objc-header -o %t/Base.swiftmodule %S/Inputs/resolve-cross-language/Base.swift -Xfrontend -disable-objc-attr-requires-foundation-module
// RUN: cp %S/Inputs/resolve-cross-language/Base-module.map %t/module.map
// RUN: %swift %clang-importer-sdk -module-cache-path %t -parse -I %t -F %S/Inputs/resolve-cross-language %s -verify
// XFAIL: linux

import Base
import BaseUser

// Sanity check.
useBaseClass(getBaseClass())
useBaseClassObjC(getBaseClassObjC())
useBaseProtoObjC(getBaseProtoObjC())
var be: BaseEnum = getBaseEnum()
useBaseEnum(be)
be = getBaseClass().baseEnumMethod(be)
be = AnotherClass().getEnum()
var beo: BaseEnumObjC = getBaseEnumObjC()
useBaseEnumObjC(beo)

// Check type resolution.
useBaseClass(getBaseClassObjC())
useBaseClassObjC(getBaseClass())
getBaseClass().categoryMethod()

useBaseProto(getBaseProtoObjC())
let p: BaseProto = UserClass()
useBaseProtoObjC(p)

getBaseClass().extensionMethod()

be = BaseEnum.Zim
be = BaseEnum.Zang
be = BaseEnum.Zung

var beRaw: CShort = be.rawValue

beo = BaseEnumObjC.Zippity
beo = BaseEnumObjC.Doo
beo = BaseEnumObjC.Dah

var beoRaw: CUnsignedChar = beo.rawValue

// Make sure we're actually parsing stuff.
useBaseClass() // expected-error{{missing argument for parameter #1}}
