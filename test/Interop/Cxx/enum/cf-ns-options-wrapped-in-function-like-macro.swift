// RUN: %target-swift-frontend -typecheck -verify %s -I %S/Inputs -cxx-interoperability-mode=default

// Test that we correctly import CF_OPTIONS/NS_OPTIONS typedef'd through a
// function-like wrapper macro as Swift OptionSet types.

import WrappedCFNSOptions

// Single function-like wrappers

printCFOpts(.foo)
printCFOpts(.bar)
printCFOpts([.foo, .bar])
printCFOpts(MyCFOpts())
let cfResult: MyCFOpts = makeCFOpts()
_ = cfResult

printNSOpts(.foo)
printNSOpts(.bar)
printNSOpts([.foo, .bar])
printNSOpts(MyNSOpts())
let nsResult: MyNSOpts = makeNSOpts()
_ = nsResult

// Two levels of function-like wrappers

printCFOpts2(.foo)
printCFOpts2(.bar)
printCFOpts2([.foo, .bar])
printCFOpts2(MyCFOpts2())
let cfResult2: MyCFOpts2 = makeCFOpts2()
_ = cfResult2

printNSOpts2(.foo)
printNSOpts2(.bar)
printNSOpts2([.foo, .bar])
printNSOpts2(MyNSOpts2())
let nsResult2: MyNSOpts2 = makeNSOpts2()
_ = nsResult2

// Object-like outer, function-like inner

printCFOptsObj(.foo)
printCFOptsObj(.bar)
printCFOptsObj([.foo, .bar])
printCFOptsObj(MyCFOptsObj())
let cfResultObj: MyCFOptsObj = makeCFOptsObj()
_ = cfResultObj

printNSOptsObj(.foo)
printNSOptsObj(.bar)
printNSOptsObj([.foo, .bar])
printNSOptsObj(MyNSOptsObj())
let nsResultObj: MyNSOptsObj = makeNSOptsObj()
_ = nsResultObj

// Function-like outer, object-like inner

printCFOptsFunc(.foo)
printCFOptsFunc(.bar)
printCFOptsFunc([.foo, .bar])
printCFOptsFunc(MyCFOptsFunc())
let cfResultFunc: MyCFOptsFunc = makeCFOptsFunc()
_ = cfResultFunc

printNSOptsFunc(.foo)
printNSOptsFunc(.bar)
printNSOptsFunc([.foo, .bar])
printNSOptsFunc(MyNSOptsFunc())
let nsResultFunc: MyNSOptsFunc = makeNSOptsFunc()
_ = nsResultFunc
