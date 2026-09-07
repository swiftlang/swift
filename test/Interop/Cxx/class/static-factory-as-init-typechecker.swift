// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default

import StaticFactoryAsInit

let nonTemplate = NonTemplateFactory(fromInt: 1)
let _: CInt = nonTemplate.value

let defaultedTemplate = DefaultedTemplateParamFactory(fromDefaultedTemplate: 2)
let _: CInt = defaultedTemplate.value

let overload = OverloadedFactories(overload: 3)
let _: CInt = overload.value

let overloadExtra = OverloadedFactories(overload: 4, extra: 5)
let _: CInt = overloadExtra.value

// The real C++ constructor and the static factory coexist.
let fromConstructor = FactoryAndConstructor(6)
let _: CInt = fromConstructor.value

let fromFactory = FactoryAndConstructor(fromFactory: 7)
let _: CInt = fromFactory.value
