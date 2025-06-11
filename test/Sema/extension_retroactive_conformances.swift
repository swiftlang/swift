// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -swift-version 5 -DLIBRARY %s -emit-module -module-name Library -o %t/Library.swiftmodule
// RUN: %target-typecheck-verify-swift -swift-version 5 -I %t

#if LIBRARY
// Define a couple protocols with no requirements that're easy to conform to
public protocol SampleProtocol1 {}
public protocol SampleProtocol2 {}
public protocol SampleProtocol1a: SampleProtocol1 {}
public protocol SampleProtocol1b: SampleProtocol1 {}

public struct Sample1 {}
public struct Sample2 {}
public struct Sample2a {}
public struct Sample2b {}
public struct Sample2c {}
public struct Sample2d {}
public struct Sample2e {}
public struct Sample3 {}
public struct Sample4 {}
public struct Sample5 {}
public struct Sample6 {}
public struct Sample7 {}
public struct Sample8 {}

public struct SampleAlreadyConforms: SampleProtocol1 {}

public struct GenericSample1<T> {}

#else

import Library

extension Sample1: SampleProtocol1 {} // expected-warning {{extension declares a conformance of imported type 'Sample1' to imported protocol 'SampleProtocol1'}}
// expected-note @-1 {{add '@retroactive' to silence this warning}} {{20-35=@retroactive SampleProtocol1}}

protocol InheritsSampleProtocol: SampleProtocol1 {}
protocol NestedInheritsSampleProtocol: InheritsSampleProtocol {}

protocol InheritsMultipleSampleProtocols: SampleProtocol1 {}
protocol NestedInheritsMultipleSampleProtocols: InheritsSampleProtocol, SampleProtocol2 {}

extension Sample2: InheritsSampleProtocol {} // expected-warning {{extension declares a conformance of imported type 'Sample2' to imported protocol 'SampleProtocol1'}}
// expected-note @-1 {{add '@retroactive' to silence this warning}} {{1-1=extension Sample2: @retroactive SampleProtocol1 {\}\n}}

extension SampleAlreadyConforms: InheritsSampleProtocol {} // ok, SampleAlreadyConforms already conforms in the source module

extension Sample2a: @retroactive SampleProtocol1, InheritsSampleProtocol {} // ok, the concrete conformance to SampleProtocol1 has been declared retroactive

extension Sample2b: InheritsSampleProtocol, @retroactive SampleProtocol1 {} // ok, same as above but in the opposite order

// FIXME: It would be better to only suggest marking SampleProtocol1a @retroactive here
extension Sample2c: SampleProtocol1a {} // expected-warning {{extension declares a conformance of imported type 'Sample2c' to imported protocols 'SampleProtocol1a', 'SampleProtocol1'}}
// expected-note @-1 {{add '@retroactive' to silence this warning}} {{21-37=@retroactive SampleProtocol1a}} {{1-1=extension Sample2c: @retroactive SampleProtocol1 {\}\n}}

extension Sample2d: @retroactive SampleProtocol1a {} // ok, retroactive conformance to SampleProtocol1a covers conformance to SampleProtocol1

extension Sample2e: SampleProtocol1a, @retroactive SampleProtocol1b {} // expected-warning {{extension declares a conformance of imported type 'Sample2e' to imported protocol 'SampleProtocol1a'}}
// expected-note @-1 {{add '@retroactive' to silence this warning}} {{21-37=@retroactive SampleProtocol1a}}

extension Sample3: NestedInheritsSampleProtocol {} // expected-warning {{extension declares a conformance of imported type 'Sample3' to imported protocol 'SampleProtocol1'}}
// expected-note @-1 {{add '@retroactive' to silence this warning}} {{1-1=extension Sample3: @retroactive SampleProtocol1 {\}\n}}

extension Sample4: NestedInheritsMultipleSampleProtocols {} // expected-warning {{extension declares a conformance of imported type 'Sample4' to imported protocols 'SampleProtocol2', 'SampleProtocol1'}}
// expected-note @-1 {{add '@retroactive' to silence this warning}} {{1-1=extension Sample4: @retroactive SampleProtocol2 {\}\nextension Sample4: @retroactive SampleProtocol1 {\}\n}}

extension Sample5: @retroactive SampleProtocol2, @retroactive SampleProtocol1 {}

// ok, explicit @retroactive in previous extension silences the warning
extension Sample5: NestedInheritsMultipleSampleProtocols {}

// Check that looking through typealiases replaces the underlying type

typealias MySample6 = Sample6

extension MySample6: SampleProtocol1 {} // expected-warning {{extension declares a conformance of imported type 'Sample6' to imported protocol 'SampleProtocol1'}}
// expected-note @-1 {{add '@retroactive' to silence this warning}} {{22-37=@retroactive SampleProtocol1}}

// Ensure module-qualifying both types still silences the warning

extension Library.Sample6: Library.SampleProtocol2 {} // ok, module-qualified.

protocol ClientProtocol {}

// ok, conforming a type from another module to a protocol within this module is totally fine
extension Sample1: ClientProtocol {}

struct MySample7: @retroactive SampleProtocol1 {} // expected-error {{'retroactive' attribute only applies in inheritance clauses in extensions}}

extension MySample7: @retroactive ClientProtocol {} // expected-warning {{'retroactive' attribute does not apply; 'MySample7' is declared in this module}}{{22-35=}}

extension Int: @retroactive ClientProtocol {} // expected-warning {{'retroactive' attribute does not apply; 'ClientProtocol' is declared in this module}}{{16-29=}}

func f(_ x: @retroactive Int) {} // expected-error {{'retroactive' attribute only applies in inheritance clauses in extensions}}

var x: @retroactive Int { 0 } // expected-error {{'retroactive' attribute only applies in inheritance clauses in extensions}}

#if os(macOS)

@available(macOS 11, *)
@_originallyDefinedIn(module: "Library", macOS 14)
public struct OriginallyDefinedInLibrary {}

@available(macOS 14, *)
extension OriginallyDefinedInLibrary: SampleProtocol1 {} // ok, @_originallyDefinedIn attribute makes this authoritative

#endif

// conditional conformances
extension GenericSample1: SampleProtocol1 where T: SampleProtocol1 {}
// expected-warning@-1 {{extension declares a conformance of imported type 'GenericSample1' to imported protocol 'SampleProtocol1'; this will not behave correctly if the owners of 'Library' introduce this conformance in the future}}
// expected-note@-2 {{add '@retroactive' to silence this warning}}

// Don't forget about protocol compositions
extension Sample7: SampleProtocol1 & SampleProtocol2 {}
// expected-warning@-1 {{extension declares a conformance of imported type 'Sample7' to imported protocols 'SampleProtocol1', 'SampleProtocol2'; this will not behave correctly if the owners of 'Library' introduce this conformance in the future}}
// expected-note@-2 {{add '@retroactive' to silence this warning}}

extension Sample8: @retroactive SampleProtocol1 & SampleProtocol2 {}  // ok

#endif
