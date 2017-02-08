// RUN: rm -rf %t
// RUN: mkdir -p %t

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource) -emit-module -emit-objc-header -o %t %S/Inputs/resolve-cross-language/Base.swift -disable-objc-attr-requires-foundation-module
// RUN: cp %S/Inputs/resolve-cross-language/Base-module.map %t/module.map
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource) -typecheck -I %t -F %S/Inputs/resolve-cross-language %s -verify

// REQUIRES: objc_interop

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
var se: SwiftEnum = getRenamedEnum()
useRenamedEnum(se)
se = getBaseClass().renamedEnumMethod(se)
se = AnotherClass().getSwiftEnum()
var seo: SwiftEnumObjC = getRenamedEnumObjC()
useRenamedEnumObjC(seo)

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

beo = BaseEnumObjC.zippity
beo = BaseEnumObjC.doo
beo = BaseEnumObjC.dah

var beoRaw: CUnsignedChar = beo.rawValue

se = SwiftEnum.Quux
se = SwiftEnum.Corge
se = SwiftEnum.Grault

var seRaw: CShort = se.rawValue

seo = SwiftEnumObjC.quux
seo = SwiftEnumObjC.corge
seo = SwiftEnumObjC.grault

var seoRaw: CUnsignedChar = seo.rawValue
_ = ExtendsTwoProtosImpl.self

// Make sure we're actually parsing stuff.
useBaseClass() // expected-error{{missing argument for parameter #1}}
