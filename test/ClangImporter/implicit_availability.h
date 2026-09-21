// REQUIRES: objc_interop
// REQUIRES: OS=macosx

// RUN: %target-swift-ide-test -print-module -module-to-print=__ObjC -import-objc-header %s -source-filename=x -target %target-cpu-apple-macosx13.0 -Xcc -Werror -Xcc -Wavailability | %FileCheck %s

// Check how the printed interface renders availability that comes from three
// different sources in the same Clang module:
//   1. An explicit Clang `availability` attribute.
//   2. The implicit Swift 5.8 runtime availability synthesized for foreign
//      reference types (and how it combines with an explicit attribute).
//   3. Availability of a protocol requirement mirrored into an adopting class.
// The deployment target is pinned below 13.3 so the implicit foreign-reference
// availability is actually emitted.

#define REF \
    __attribute__((swift_attr("import_reference"))) \
    __attribute__((swift_attr("retain:immortal")))  \
    __attribute__((swift_attr("release:immortal")))
#define AVAIL(...) __attribute__((availability(__VA_ARGS__)))

AVAIL(macos, introduced=15.0)
@interface ExplicitlyAvailable
- (void)doThing;
@end
// An explicit availability attribute is imported directly.
// CHECK:      @available(macOS 15.0, *)
// CHECK-NEXT: class ExplicitlyAvailable {
// CHECK-NEXT:   class func doThing()
// CHECK-NEXT:   func doThing()
// CHECK-NEXT: }

AVAIL(macos, introduced=14.0, deprecated=15.0)
@interface ExplicitlyAvailableDe
- (void)doThing;
@end
// CHECK-NEXT: @available(macOS, introduced: 14.0, deprecated: 15.0)
// CHECK-NEXT: class ExplicitlyAvailableDe {
// CHECK-NEXT:   class func doThing()
// CHECK-NEXT:   func doThing()
// CHECK-NEXT: }

AVAIL(macos, introduced=14.0)
AVAIL(macos, deprecated=15.0)
@interface ExplicitlyAvailableDepSeparate
- (void)doThing;
@end
// CHECK-NEXT: @available(macOS, introduced: 14.0, deprecated: 15.0)
// CHECK-NEXT: class ExplicitlyAvailableDepSeparate {
// CHECK-NEXT:   class func doThing()
// CHECK-NEXT:   func doThing()
// CHECK-NEXT: }

AVAIL(macos, introduced=14.0)
AVAIL(anyAppleOS, introduced=26.0, deprecated=26.2)
@interface ExplicitlyAvailableDepSeparateAny
- (void)doThing;
@end
// CHECK-NEXT: @available(macOS 14.0, *)
// CHECK-NEXT: class ExplicitlyAvailableDepSeparateAny {
// CHECK-NEXT:   class func doThing()
// CHECK-NEXT:   func doThing()
// CHECK-NEXT: }

struct REF RefType {};
// A foreign reference type gets the implicit Swift 5.8 runtime availability.
// CHECK-NEXT: @available(macOS 13.3.0, *)
// CHECK-NEXT: class RefType {
// CHECK-NEXT: }

struct REF AVAIL(macos, introduced=26.0) LateRefType {};
// A foreign reference type with an explicit (later) availability merges
// the implicit runtime availability and the explicit one.
// CHECK-NEXT: @available(macOS 26.0, *)
// CHECK-NEXT: class LateRefType {
// CHECK-NEXT: }

struct REF AVAIL(macos, deprecated=15.0, message="gone") DeprecatedRef {};
// CHECK-NEXT: @available(macOS, introduced: 13.3.0, deprecated: 15.0, message: "gone")
// CHECK-NEXT: class DeprecatedRef {
// CHECK-NEXT: }

struct REF AVAIL(macos, unavailable, message="nope") UnavailableRef {};
// CHECK-NEXT: @available(macOS, unavailable, message: "nope")
// CHECK-NEXT: class UnavailableRef {
// CHECK-NEXT: }

struct REF AVAIL(macos, introduced=14.0, deprecated=15.0) LateIntroDepRef {};
// CHECK-NEXT: @available(macOS, introduced: 14.0, deprecated: 15.0)
// CHECK-NEXT: class LateIntroDepRef {
// CHECK-NEXT: }

struct REF AVAIL(macos, introduced=12.0, deprecated=15.0) EarlyIntroLateDepRef {};
// CHECK-NEXT: @available(macOS, introduced: 13.3.0, deprecated: 15.0)
// CHECK-NEXT: class EarlyIntroLateDepRef {
// CHECK-NEXT: }

struct REF AVAIL(macos, introduced=12.0, deprecated=13.0) EarlyIntroEarlyDepRef {};
// CHECK-NEXT: @available(macOS, introduced: 13.3.0, deprecated: 13.0)
// CHECK-NEXT: class EarlyIntroEarlyDepRef {
// CHECK-NEXT: }

struct REF AVAIL(macos, introduced=12.0) AVAIL(macos, deprecated=13.0) EarlyIntroEarlyDepRefSeparate {};
// CHECK-NEXT: @available(macOS, introduced: 13.3.0, deprecated: 13.0)
// CHECK-NEXT: class EarlyIntroEarlyDepRefSeparate {
// CHECK-NEXT: }

struct REF AVAIL(macos, deprecated=13.0) DepOnlyEarly {};
// CHECK-NEXT: @available(macOS, introduced: 13.3.0, deprecated: 13.0)
// CHECK-NEXT: class DepOnlyEarly {
// CHECK-NEXT: }

struct REF AVAIL(macos, deprecated=14.0) DepOnlyLate {};
// CHECK-NEXT: @available(macOS, introduced: 13.3.0, deprecated: 14.0)
// CHECK-NEXT: class DepOnlyLate {
// CHECK-NEXT: }

AVAIL(macos, introduced=14.0)
@protocol AvailProto
- (void)fromAvailableProtocol;
- (void)fromAvailableProtocolExplicit AVAIL(macos, introduced=15.0);
- (void)fromAvailableProtocolExplicitAny AVAIL(anyAppleOS, introduced=26.0);
- (void)fromAvailableProtocolExplicitLow AVAIL(macos, introduced=13.0);
@end
// CHECK-NEXT: @available(macOS 14.0, *)
// CHECK-NEXT: protocol AvailProto {
// CHECK-NEXT:   func fromAvailableProtocol()
// CHECK-NEXT:   @available(macOS 15.0, *)
// CHECK-NEXT:   func fromAvailableProtocolExplicit()
// CHECK-NEXT:   @available(anyAppleOS 26.0, *)
// CHECK-NEXT:   func fromAvailableProtocolExplicitAny()
// CHECK-NEXT:   @available(macOS 13.0, *)
// CHECK-NEXT:   func fromAvailableProtocolExplicitLow()
// CHECK-NEXT: }

@interface Adopter <AvailProto>
@end
// CHECK-NEXT: class Adopter : AvailProto {
// Explicit is protocol availability is propagated to the
// copy mirrored into an adopting class.
// CHECK-NEXT:   @available(macOS 14.0, *)
// CHECK-NEXT:   func fromAvailableProtocol()
// CHECK-NEXT:   @available(macOS 14.0, *)
// CHECK-NEXT:   class func fromAvailableProtocol()
// Subsumed availability is NOT copied.
// CHECK-NEXT:   @available(macOS 15.0, *)
// CHECK-NEXT:   func fromAvailableProtocolExplicit()
// CHECK-NEXT:   @available(macOS 15.0, *)
// CHECK-NEXT:   class func fromAvailableProtocolExplicit()
// CHECK-NEXT:   @available(anyAppleOS 26.0, *)
// CHECK-NEXT:   func fromAvailableProtocolExplicitAny()
// CHECK-NEXT:   @available(anyAppleOS 26.0, *)
// CHECK-NEXT:   class func fromAvailableProtocolExplicitAny()
// CHECK-NEXT:   @available(macOS 13.0, *)
// CHECK-NEXT:   func fromAvailableProtocolExplicitLow()
// CHECK-NEXT:   @available(macOS 13.0, *)
// CHECK-NEXT:   class func fromAvailableProtocolExplicitLow()
// CHECK-NEXT: }

AVAIL(anyAppleOS, introduced=26.0)
@protocol AvailProtoAny
- (void)fromAvailableProtocol;
- (void)fromAvailableProtocolExplicit AVAIL(macos, introduced=27.0);
- (void)fromAvailableProtocolExplicitAny AVAIL(anyAppleOS, introduced=27.0);
- (void)fromAvailableProtocolExplicitLow AVAIL(macos, introduced=13.0);
- (void)fromAvailableProtocolExplicitIos AVAIL(ios, introduced=27.0);
- (void)fromAvailableProtocolExplicitIosLow AVAIL(ios, introduced=15.0);
@end
// Also works for anyAppleOS.
// CHECK-NEXT: @available(anyAppleOS 26.0, *)
// CHECK-NEXT: protocol AvailProtoAny {
// CHECK-NEXT:   func fromAvailableProtocol()
// CHECK-NEXT:   @available(macOS 27.0, *)
// CHECK-NEXT:   func fromAvailableProtocolExplicit()
// CHECK-NEXT:   @available(anyAppleOS 27.0, *)
// CHECK-NEXT:   func fromAvailableProtocolExplicitAny()
// FIXME? Earlier availability than the surrounding context would be an error
// in Swift, but clang allows it.
// CHECK-NEXT:   @available(macOS 13.0, *)
// CHECK-NEXT:   func fromAvailableProtocolExplicitLow()
// Another OSs availability is ignored
// CHECK-NEXT:   func fromAvailableProtocolExplicitIos()
// CHECK-NEXT:   func fromAvailableProtocolExplicitIosLow()
// CHECK-NEXT: }

@interface AdopterAny <AvailProtoAny>
@end
// CHECK-NEXT: class AdopterAny : AvailProtoAny {
// CHECK-NEXT:   @available(macOS 26.0, *)
// CHECK-NEXT:   func fromAvailableProtocol()
// CHECK-NEXT:   @available(macOS 26.0, *)
// CHECK-NEXT:   class func fromAvailableProtocol()
// CHECK-NEXT:   @available(macOS 27.0, *)
// CHECK-NEXT:   func fromAvailableProtocolExplicit()
// CHECK-NEXT:   @available(macOS 27.0, *)
// CHECK-NEXT:   class func fromAvailableProtocolExplicit()
// CHECK-NEXT:   @available(anyAppleOS 27.0, *)
// CHECK-NEXT:   func fromAvailableProtocolExplicitAny()
// CHECK-NEXT:   @available(anyAppleOS 27.0, *)
// CHECK-NEXT:   class func fromAvailableProtocolExplicitAny()
// CHECK-NEXT:   @available(macOS 13.0, *)
// CHECK-NEXT:   func fromAvailableProtocolExplicitLow()
// CHECK-NEXT:   @available(macOS 13.0, *)
// CHECK-NEXT:   class func fromAvailableProtocolExplicitLow()
// Functions with explicit annotations for another OS still inherit from protocol
// CHECK-NEXT:   @available(macOS 26.0, *)
// CHECK-NEXT:   func fromAvailableProtocolExplicitIos()
// CHECK-NEXT:   @available(macOS 26.0, *)
// CHECK-NEXT:   class func fromAvailableProtocolExplicitIos()
// CHECK-NEXT:   @available(macOS 26.0, *)
// CHECK-NEXT:   func fromAvailableProtocolExplicitIosLow()
// CHECK-NEXT:   @available(macOS 26.0, *)
// CHECK-NEXT:   class func fromAvailableProtocolExplicitIosLow()
// CHECK-NEXT: }
