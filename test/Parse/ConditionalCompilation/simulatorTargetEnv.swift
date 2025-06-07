// RUN: %swift -swift-version 4 -typecheck %s -verify -target x86_64-apple-ios7.0-simulator -parse-stdlib
// RUN: %swift -swift-version 4 -typecheck %s -verify -target x86_64-unknown-linux-simulator -parse-stdlib
// RUN: %swift-ide-test -swift-version 4 -test-input-complete -source-filename=%s -target x86_64-apple-ios7.0-simulator

#if !targetEnvironment(simulator)
// This block should not parse.
let i: Int = "Hello"
#endif

#if targetEnvironment(simulator)
class C {}
var x = C()
#endif
var y = x

#if os(iOS) && arch(i386)
// expected-warning @-1 {{platform condition appears to be testing for simulator environment}}
// expected-note@-2{{replace with 'targetEnvironment(simulator)'}}{{5-26=targetEnvironment(simulator)}}
class C1 {}
#endif

#if arch(i386) && os(iOS)
// expected-warning @-1 {{platform condition appears to be testing for simulator environment}}
// expected-note@-2{{replace with 'targetEnvironment(simulator)'}}{{5-26=targetEnvironment(simulator)}}
class C2 {}
#endif

#if arch(i386) && (os(iOS) || os(watchOS))
// expected-warning @-1 {{platform condition appears to be testing for simulator environment}}
// expected-note@-2{{replace with 'targetEnvironment(simulator)'}}{{5-43=targetEnvironment(simulator)}}
class C3 {}
#endif

#if (arch(x86_64) || arch(i386)) && (os(iOS) || os(watchOS) || os(tvOS))
// expected-warning @-1 {{platform condition appears to be testing for simulator environment}}
// expected-note@-2{{replace with 'targetEnvironment(simulator)'}}{{5-73=targetEnvironment(simulator)}}
class C4 {}
#endif

#if !(arch(x86_64) && os(tvOS))
// expected-warning @-1 {{platform condition appears to be testing for simulator environment}}
// expected-note@-2{{replace with 'targetEnvironment(simulator)'}}{{7-31=targetEnvironment(simulator)}}
class C5 {}
#endif
