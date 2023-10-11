// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -swift-version 5 -DLIBRARY %s -emit-module -module-name Library -o %t/Library.swiftmodule
// RUN: %target-typecheck-verify-swift -swift-version 5 -I %t

#if LIBRARY
// Define a couple protocols with no requirements that're easy to conform to
public protocol SampleProtocol1 {}
public protocol SampleProtocol2 {}

public struct Sample1 {}
public struct Sample2 {}
public struct Sample3 {}
public struct Sample4 {}
public struct Sample5 {}
public struct Sample6 {}

public struct SampleAlreadyConforms: SampleProtocol1 {}
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

struct Sample7: @retroactive SampleProtocol1 {} // expected-error {{'retroactive' attribute only applies in inheritance clauses in extensions}}

extension Sample7: @retroactive ClientProtocol {} // expected-error {{'retroactive' attribute does not apply; 'Sample7' is declared in this module}}

extension Int: @retroactive ClientProtocol {} // expected-error {{'retroactive' attribute does not apply; 'ClientProtocol' is declared in this module}}

func f(_ x: @retroactive Int) {} // expected-error {{'retroactive' attribute only applies in inheritance clauses in extensions}}

var x: @retroactive Int { 0 } // expected-error {{'retroactive' attribute only applies in inheritance clauses in extensions}}

#if os(macOS)

@available(macOS 11, *)
@_originallyDefinedIn(module: "Library", macOS 14)
public struct OriginallyDefinedInLibrary {}

@available(macOS 14, *)
extension OriginallyDefinedInLibrary: SampleProtocol1 {} // ok, @_originallyDefinedIn attribute makes this authoritative

#endif

#endif
