// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -swift-version 5 -DLIBRARY %s -emit-module -module-name Library -o %t/Library.swiftmodule
// RUN: %target-typecheck-verify-swift -swift-version 5 -I %t -verify-additional-file Library.SampleComposition3 -verify-additional-file Library.SampleComposition4

#if LIBRARY
// Define a couple protocols with no requirements that're easy to conform to
public protocol SampleProtocol1 {}
public protocol SampleProtocol2 {}
public protocol SampleProtocol1a: SampleProtocol1 {}
public protocol SampleProtocol1b: SampleProtocol1 {}
public protocol SampleProtocol1aa: SampleProtocol1a {}

public protocol SampleProtocol3<A> {
  associatedtype A
}

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
public struct Sample6a {}
public struct Sample6b {}
public struct Sample6c {}
public struct Sample7 {}
public struct Sample8 {}
public struct Sample8a {}

public struct SampleAlreadyConforms: SampleProtocol1 {}

public struct GenericSample1<T> {}

public struct Sample9 {}
public struct Sample9a {}
public struct Sample9b {}

public struct SampleComposition1 {}
public struct SampleComposition2 {}
public struct SampleComposition3 {}
public struct SampleComposition4 {}
public struct SampleParameterized1 {}
public struct SampleParameterized2 {}
public struct SampleTransitive1 {}
public struct SampleUnavailable1 {}
public struct SampleUnavailable2 {}
public struct SampleUnavailable3 {}
public struct SampleUnavailable4 {}
public struct SampleComposition5 {}

@available(*, unavailable)
extension SampleComposition1: SampleProtocol1 & SampleProtocol2 {}

@available(*, unavailable)
extension SampleComposition2: SampleProtocol1 & SampleProtocol2 {}

@available(*, unavailable)
extension SampleComposition3: SampleProtocol1a & SampleProtocol1b {}

@available(*, unavailable)
extension SampleComposition4: SampleProtocol1 & SampleProtocol2 {}

@available(*, unavailable)
extension SampleParameterized1: SampleProtocol3<Int> {}

@available(*, unavailable)
extension SampleParameterized2: SampleProtocol3<Int> & SampleProtocol1 {}

@available(*, unavailable)
extension SampleTransitive1: SampleProtocol1aa {}

@available(*, unavailable)
extension SampleUnavailable1: SampleProtocol1 {}

@available(*, unavailable)
extension SampleUnavailable2: SampleProtocol1 {}

@available(*, unavailable)
extension SampleUnavailable2: SampleProtocol2 {}

@available(*, unavailable)
extension SampleUnavailable3: SampleProtocol1 {}

@available(*, unavailable)
extension SampleUnavailable3: SampleProtocol2 {}

@available(*, unavailable)
extension SampleUnavailable4: SampleProtocol1 {}

@available(*, unavailable)
extension SampleComposition5: SampleProtocol1 & SampleProtocol2 {}

@_semantics("fast_cast")
public protocol FastCast {}

public class C {}

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

// Ensure module-qualifying the protocol silences the warning

extension Library.Sample6: Library.SampleProtocol2 {} // ok, both types are module-qualified.
extension Sample6a: Library.SampleProtocol2 {} // ok, protocol is module qualified.
extension Library.Sample6b: SampleProtocol2 {} // expected-warning {{extension declares a conformance of imported type 'Sample6b' to imported protocol 'SampleProtocol2'; this will not behave correctly if the owners of 'Library' introduce this conformance in the future}}
// expected-note @-1 {{add '@retroactive' to silence this warning}}
extension Sample6c: Library.SampleProtocol1a {} // ok, protocol is module qualified.

protocol ClientProtocol {}

// ok, conforming a type from another module to a protocol within this module is totally fine
extension Sample1: ClientProtocol {}

struct MySample7: @retroactive SampleProtocol1 {} // expected-error {{'@retroactive' only applies in inheritance clauses in extensions}}{{19-32=}}

extension MySample7: @retroactive ClientProtocol {} // expected-warning {{'retroactive' attribute does not apply; 'MySample7' is declared in this module}}{{22-35=}}

extension Int: @retroactive ClientProtocol {} // expected-warning {{'retroactive' attribute does not apply; 'ClientProtocol' is declared in this module}}{{16-29=}}

func f(_ x: @retroactive Int) {} // expected-error {{'@retroactive' only applies in inheritance clauses in extensions}}

var x: @retroactive Int { 0 } // expected-error {{'@retroactive' only applies in inheritance clauses in extensions}}

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

// FIXME: Module qualification should suppress this warning
extension Sample8a: Library.SampleProtocol1 & Library.SampleProtocol2 {}  // ok
// expected-warning@-1 {{extension declares a conformance of imported type 'Sample8a' to imported protocols 'SampleProtocol1', 'SampleProtocol2'; this will not behave correctly if the owners of 'Library' introduce this conformance in the future}}
// expected-note@-2 {{add '@retroactive' to silence this warning}}

extension Sample9: SampleProtocol3<Int> {}
// expected-warning@-1 {{extension declares a conformance of imported type 'Sample9' to imported protocol 'SampleProtocol3'; this will not behave correctly if the owners of 'Library' introduce this conformance in the future}}
// expected-note@-2 {{add '@retroactive' to silence this warning}}

extension Sample9a: @retroactive SampleProtocol3<Int> {}

// FIXME: Module qualification should suppress this warning
extension Sample9b: Library.SampleProtocol3<Int> {}
// expected-warning@-1 {{extension declares a conformance of imported type 'Sample9b' to imported protocol 'SampleProtocol3'; this will not behave correctly if the owners of 'Library' introduce this conformance in the future}}
// expected-note@-2 {{add '@retroactive' to silence this warning}}

// Unavailable protocol composition conformances should not suggest @retroactive.
extension SampleComposition1: SampleProtocol1 {}
// expected-warning@-1 {{extension declares a conformance of imported type 'SampleComposition1' to imported protocol 'SampleProtocol1'; the owners of 'Library' have marked this conformance as unavailable}}

extension SampleComposition2: SampleProtocol1 & SampleProtocol2 {}
// expected-warning@-1 {{extension declares a conformance of imported type 'SampleComposition2' to imported protocols 'SampleProtocol1', 'SampleProtocol2'; the owners of 'Library' have marked these conformances as unavailable}}

// SampleProtocol1a & SampleProtocol1b are unavailable via composition, including inherited SampleProtocol1.
// TODO: SampleComposition3 and 4 are arguably too noisy, look into how we could make this more clear and concise.
extension SampleComposition3: SampleProtocol1a {} // expected-error {{conformance of 'SampleComposition3' to 'SampleProtocol1' is unavailable}}
// expected-note@Library.SampleComposition3:2 {{conformance of 'SampleComposition3' to 'SampleProtocol1' has been explicitly marked unavailable here}}
// expected-warning@-2 {{extension declares a conformance of imported type 'SampleComposition3' to imported protocol 'SampleProtocol1a'; the owners of 'Library' have marked this conformance as unavailable}}

// SampleProtocol1 and SampleProtocol2 are unavailable. SampleProtocol1a is not, so it gets the retroactive warning.
extension SampleComposition4: SampleProtocol2 & SampleProtocol1a {} // expected-error {{conformance of 'SampleComposition4' to 'SampleProtocol1' is unavailable}}
// expected-note@Library.SampleComposition4:2 {{conformance of 'SampleComposition4' to 'SampleProtocol1' has been explicitly marked unavailable here}}
// expected-warning@-2 {{extension declares a conformance of imported type 'SampleComposition4' to imported protocol 'SampleProtocol2'; the owners of 'Library' have marked this conformance as unavailable}}
// expected-warning@-3 {{extension declares a conformance of imported type 'SampleComposition4' to imported protocol 'SampleProtocol1a'; this will not behave correctly if the owners of 'Library' introduce this conformance in the future}}
// expected-note@-4 {{add '@retroactive' to silence this warning}} {{1-1=extension SampleComposition4: @retroactive SampleProtocol1a {\}\n}}

// Unavailable parameterized protocol conformances should also not suggest @retroactive.
extension SampleParameterized1: SampleProtocol3<Int> {}
// expected-warning@-1 {{extension declares a conformance of imported type 'SampleParameterized1' to imported protocol 'SampleProtocol3'; the owners of 'Library' have marked this conformance as unavailable}}

// Parameterized protocols in unavailable compositions should also be handled.
extension SampleParameterized2: SampleProtocol3<Int> & SampleProtocol1 {}
// expected-warning@-1 {{extension declares a conformance of imported type 'SampleParameterized2' to imported protocols 'SampleProtocol3', 'SampleProtocol1'; the owners of 'Library' have marked these conformances as unavailable}}

// Transitive inherited protocols should also be recognized as unavailable.
extension SampleTransitive1: SampleProtocol1 {}
// expected-warning@-1 {{extension declares a conformance of imported type 'SampleTransitive1' to imported protocol 'SampleProtocol1'; the owners of 'Library' have marked this conformance as unavailable}}

// Single-protocol unavailable conformance.
extension SampleUnavailable1: SampleProtocol1 {}
// expected-warning@-1 {{extension declares a conformance of imported type 'SampleUnavailable1' to imported protocol 'SampleProtocol1'; the owners of 'Library' have marked this conformance as unavailable}}

// Multiple separate unavailable extensions on the same type, conformed via composition.
extension SampleUnavailable2: SampleProtocol1 & SampleProtocol2 {}
// expected-warning@-1 {{extension declares a conformance of imported type 'SampleUnavailable2' to imported protocols 'SampleProtocol1', 'SampleProtocol2'; the owners of 'Library' have marked these conformances as unavailable}}

// Comma-separated conformance to unavailable protocols.
extension SampleUnavailable3: SampleProtocol1, SampleProtocol2 {}
// expected-warning@-1 {{extension declares a conformance of imported type 'SampleUnavailable3' to imported protocols 'SampleProtocol1', 'SampleProtocol2'; the owners of 'Library' have marked these conformances as unavailable}}

// TODO: Should @retroactive be able to suppress the warning when we know the protocol is explicitly unavailable?
extension SampleUnavailable4: @retroactive SampleProtocol1 {}

// @retroactive on a composition suppresses the unavailable warning for all the protocols in the composition.
extension SampleComposition5: @retroactive SampleProtocol1 & SampleProtocol2 {}

extension C: FastCast {} // expected-error {{cannot add retroactive conformance for a fast-cast protocol 'FastCast'}}

#endif
