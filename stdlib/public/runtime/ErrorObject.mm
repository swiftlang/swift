//===--- ErrorObject.mm - Cocoa-interoperable recoverable error object ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This implements the object representation of the standard ErrorType protocol
// type, which represents recoverable errors in the language. This
// implementation is designed to interoperate efficiently with Cocoa libraries
// by:
// - allowing for NSError and CFError objects to "toll-free bridge" to
//   ErrorType existentials, which allows for cheap Cocoa to Swift interop
// - allowing a native Swift error to lazily "become" an NSError when
//   passed into Cocoa, allowing for cheap Swift to Cocoa interop
//
//===----------------------------------------------------------------------===//

#include "Debug.h"
#include "ErrorObject.h"
#include "Private.h"
#include <dlfcn.h>
#include <objc/objc-internal.h>
#include <objc/runtime.h>
#include <Foundation/Foundation.h>

using namespace swift;

/// A subclass of NSError used to represent bridged native Swift errors.
/// This type cannot be subclassed, and should not ever be instantiated
/// except by the Swift runtime.
@interface _SwiftNativeNSError : NSError
@end

@implementation _SwiftNativeNSError

+ (instancetype)alloc {
  swift::crash("_SwiftNativeNSError cannot be instantiated");
}

- (void)dealloc {
  // We must destroy the contained Swift value.
  auto error = (SwiftError*)self;
  error->getType()->vw_destroy(error->getValue());

  [super dealloc];
}

// Override the domain/code/userInfo accessors to follow our idea of NSError's
// layout. This gives us a buffer in case NSError decides to change its stored
// property order.

- (NSString*)domain {
  auto error = (const SwiftError*)self;
  // The domain string should not be nil; if it is, then this error box hasn't
  // been initialized yet as an NSError.
  auto domain = error->domain.load(SWIFT_MEMORY_ORDER_CONSUME);
  assert(domain
         && "ErrorType box used as NSError before initialization");
  // Don't need to .retain.autorelease since it's immutable.
  return (NSString*)domain;
}

- (NSInteger)code {
  auto error = (const SwiftError*)self;
  return error->code.load(SWIFT_MEMORY_ORDER_CONSUME);
}

- (NSDictionary*)userInfo {
  auto error = (const SwiftError*)self;
  auto userInfo = error->userInfo.load(SWIFT_MEMORY_ORDER_CONSUME);
  
  if (userInfo) {
    // Don't need to .retain.autorelease since it's immutable.
    return (NSDictionary*)userInfo;
  } else {
    // -[NSError userInfo] never returns nil on OS X 10.8 or later.
    static NSDictionary *emptyDict = @{};
    return emptyDict;
  }
}

@end

Class swift::getNSErrorClass() {
  static auto TheNSError = [NSError class];
  return TheNSError;
}

static Class getSwiftNativeNSErrorClass() {
  static auto TheSwiftNativeNSError = [_SwiftNativeNSError class];
  return TheSwiftNativeNSError;
}

/// Allocate a catchable error object.
static BoxPair::Return
_swift_allocError_(const Metadata *type,
                   const WitnessTable *errorConformance) {
  auto TheSwiftNativeNSError = getSwiftNativeNSErrorClass();
  assert(class_getInstanceSize(TheSwiftNativeNSError) == sizeof(NSErrorLayout)
         && "NSError layout changed!");
  
  // Determine the extra allocated space necessary to carry the value.
  // TODO: If the error type is a simple enum with no associated values, we
  // could emplace it in the "code" slot of the NSError and save ourselves
  // some work.
  
  unsigned size = type->getValueWitnesses()->getSize();
  unsigned alignMask = type->getValueWitnesses()->getAlignmentMask();

  size_t alignmentPadding = -sizeof(SwiftError) & alignMask;
  size_t totalExtraSize = sizeof(SwiftError) - sizeof(NSErrorLayout)
    + alignmentPadding + size;
  size_t valueOffset = alignmentPadding + sizeof(SwiftError);
  
  // Allocate the instance as if it were a CFError. We won't really initialize
  // the CFError parts until forced to though.
  auto instance
    = (SwiftError *)class_createInstance(TheSwiftNativeNSError, totalExtraSize);

  // Leave the NSError bits zero-initialized. We'll lazily instantiate them when
  // needed.

  // Initialize the Swift type metadata.
  instance->type = type;
  instance->errorConformance = errorConformance;
  
  // Return the SwiftError reference and a pointer to the uninitialized value
  // inside.
  auto valuePtr = reinterpret_cast<char*>(instance) + valueOffset;
  return BoxPair{reinterpret_cast<HeapObject*>(instance),
                 reinterpret_cast<OpaqueValue*>(valuePtr)};
}

extern "C" auto *_swift_allocError = _swift_allocError_;

BoxPair::Return
swift::swift_allocError(const Metadata *type,
                        const WitnessTable *errorConformance) {
  return _swift_allocError(type, errorConformance);
}

/// Deallocate an error object whose contained object has already been
/// destroyed.
static void
_swift_deallocError_(SwiftError *error,
                     const Metadata *type) {
  object_dispose((id)error);
}

extern "C" auto *_swift_deallocError = _swift_deallocError_;

void
swift::swift_deallocError(SwiftError *error, const Metadata *type) {
  return _swift_deallocError(error, type);
}

static const WitnessTable *getNSErrorConformanceToErrorType() {
  // CFError and NSError are toll-free-bridged, so we can use either type's
  // witness table interchangeably. CFError's is potentially slightly more
  // efficient since it doesn't need to dispatch for an unsubclassed NSCFError.
  // The witness table lives in the Foundation overlay, but it should be safe
  // to assume that that's been linked in if a user is using NSError in their
  // Swift source.
  
  static auto TheWitnessTable = dlsym(RTLD_DEFAULT,
                                   "_TWPCSo7CFErrorSs10_ErrorType10Foundation");
  assert(TheWitnessTable &&
         "Foundation overlay not loaded, or CFError: ErrorType conformance "
         "not available");
  
  return reinterpret_cast<const WitnessTable *>(TheWitnessTable);
}

bool SwiftError::isPureNSError() const {
  auto TheSwiftNativeNSError = getSwiftNativeNSErrorClass();
  // We can do an exact type check; _SwiftNativeNSError shouldn't be subclassed
  // or proxied.
  return (Class)_swift_getClass(this) != TheSwiftNativeNSError;
}

const Metadata *SwiftError::getType() const {
  if (isPureNSError()) {
    auto asError = (NSError*)this;
    return swift_getObjCClassMetadata((ClassMetadata*)[asError class]);
  }
  return type;
}

const WitnessTable *SwiftError::getErrorConformance() const {
  if (isPureNSError()) {
    return getNSErrorConformanceToErrorType();
  }
  return errorConformance;
}

/// Extract a pointer to the value, the type metadata, and the ErrorType
/// protocol witness from an error object.
///
/// The "scratch" pointer should point to an uninitialized word-sized
/// temporary buffer. The implementation may write a reference to itself to
/// that buffer if the error object is a toll-free-bridged NSError instead of
/// a native Swift error, in which case the object itself is the "boxed" value.
static void
_swift_getErrorValue_(const SwiftError *errorObject,
                      void **scratch,
                      ErrorValueResult *out) {
  // TODO: Would be great if Clang had a return-three convention so we didn't
  // need the out parameter here.

  // Check for a bridged Cocoa NSError.
  if (errorObject->isPureNSError()) {
    // Return a pointer to the scratch buffer.
    auto asError = (NSError*)errorObject;
    
    *scratch = (void*)errorObject;
    out->value = (const OpaqueValue *)scratch;
    out->type = swift_getObjCClassMetadata((ClassMetadata*)[asError class]);

    out->errorConformance = getNSErrorConformanceToErrorType();
    return;
  }
  
  out->value = errorObject->getValue();
  out->type = errorObject->type;
  out->errorConformance = errorObject->errorConformance;
  return;
}

extern "C" auto *_swift_getErrorValue = _swift_getErrorValue_;

void
swift::swift_getErrorValue(const SwiftError *errorObject,
                           void **scratch,
                           ErrorValueResult *out) {
  return _swift_getErrorValue(errorObject, scratch, out);
}

// @asmname("swift_stdlib_getErrorDomainNSString")
// public func _stdlib_getErrorDomainNSString<T : _ErrorType>
//   (x: UnsafePointer<T>) -> AnyObject
extern "C" NSString *swift_stdlib_getErrorDomainNSString(
                                                 const OpaqueValue *error,
                                                 const Metadata *T,
                                                 const WitnessTable *ErrorType);
// @asmname("swift_stdlib_getErrorCode")
// public func _stdlib_getErrorCode<T : _ErrorType>(x: UnsafePointer<T>) -> Int
extern "C" NSInteger swift_stdlib_getErrorCode(const OpaqueValue *error,
                                               const Metadata *T,
                                               const WitnessTable *ErrorType);

/// Take an ErrorType box and turn it into a valid NSError instance.
static id _swift_bridgeErrorTypeToNSError_(SwiftError *errorObject) {
  auto ns = reinterpret_cast<NSError *>(errorObject);
  // If we already have a domain set, then we've already initialized.
  if (errorObject->domain.load(SWIFT_MEMORY_ORDER_CONSUME))
    return ns;
  
  // Otherwise, calculate the domain and code (TODO: and user info), and
  // initialize the NSError.
  auto value = SwiftError::getIndirectValue(&errorObject);
  auto type = errorObject->getType();
  auto witness = errorObject->getErrorConformance();
  
  NSString *domain = swift_stdlib_getErrorDomainNSString(value, type, witness);
  NSInteger code = swift_stdlib_getErrorCode(value, type, witness);
  // TODO: user info?
  
  // The error code shouldn't change, so we can store it blindly, even if
  // somebody beat us to it. The store can be relaxed, since we'll do a
  // store(release) of the domain last thing to publish the initialized
  // NSError.
  errorObject->code.store(code, std::memory_order_relaxed);

  // However, we need to cmpxchg in the domain; if somebody beat us to it,
  // we need to release.
  //
  // Storing the domain must be the LAST THING we do, since it's
  // the signal that the NSError has been initialized.
  CFStringRef expected = nullptr;
  if (!errorObject->domain.compare_exchange_strong(expected,
                                                   (CFStringRef)domain,
                                                   std::memory_order_acq_rel))
    objc_release(domain);
  
  return ns;
}

extern "C" auto *_swift_bridgeErrorTypeToNSError = _swift_bridgeErrorTypeToNSError_;

id
swift::swift_bridgeErrorTypeToNSError(SwiftError *errorObject) {
  return _swift_bridgeErrorTypeToNSError(errorObject);
}

bool
swift::tryDynamicCastNSErrorToValue(OpaqueValue *dest,
                                    OpaqueValue *src,
                                    const Metadata *srcType,
                                    const Metadata *destType,
                                    DynamicCastFlags flags) {
  Class TheNSErrorClass = [NSError class];
  static CFTypeID TheCFErrorTypeID = CFErrorGetTypeID();
  // @asmname("swift_stdlib_bridgeNSErrorToErrorType")
  // public func _stdlib_bridgeNSErrorToErrorType<
  //   T : _ObjectiveCBridgeableErrorType
  // >(error: NSError, out: UnsafeMutablePointer<T>) -> Bool {
  static auto bridgeNSErrorToErrorType
    = reinterpret_cast<bool (*)(NSError *, OpaqueValue*, const Metadata *,
                                const WitnessTable *)>
        (dlsym(RTLD_DEFAULT, "swift_stdlib_bridgeNSErrorToErrorType"));
  // protocol _ObjectiveCBridgeableErrorType
  static auto TheObjectiveCBridgeableErrorTypeProtocol
    = reinterpret_cast<const ProtocolDescriptor *>
        (dlsym(RTLD_DEFAULT, "_TMp10Foundation30_ObjectiveCBridgeableErrorType"));

  // If the Foundation overlay isn't loaded, then NSErrors can't be bridged.
  if (!bridgeNSErrorToErrorType || !TheObjectiveCBridgeableErrorTypeProtocol)
    return false;
  
  // Is the input type an NSError?
  switch (srcType->getKind()) {
  case MetadataKind::Class:
    // Native class should be an NSError subclass.
    if (![(Class)srcType isSubclassOfClass: TheNSErrorClass])
      return false;
    break;
  case MetadataKind::ForeignClass: {
    // Foreign class should be CFError.
    CFTypeRef srcInstance = *reinterpret_cast<CFTypeRef *>(src);
    if (CFGetTypeID(srcInstance) != TheCFErrorTypeID)
      return false;
    break;
  }
  case MetadataKind::ObjCClassWrapper: {
    // ObjC class should be an NSError subclass.
    auto srcWrapper = static_cast<const ObjCClassWrapperMetadata *>(srcType);
    if (![(Class)srcWrapper->getClassObject()
            isSubclassOfClass: TheNSErrorClass])
      return false;
    break;
  }
  // Not a class.
  case MetadataKind::Enum:
  case MetadataKind::Existential:
  case MetadataKind::ExistentialMetatype:
  case MetadataKind::Function:
  case MetadataKind::ThinFunction:
  case MetadataKind::Block:
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::Metatype:
  case MetadataKind::Opaque:
  case MetadataKind::PolyFunction:
  case MetadataKind::Struct:
  case MetadataKind::Tuple:
    return false;
  }
  
  // Is the target type a bridgeable error?
  auto witness = swift_conformsToProtocol(destType,
                                      TheObjectiveCBridgeableErrorTypeProtocol);
  
  if (!witness)
    return false;
  
  // If so, attempt the bridge.
  NSError *srcInstance = *reinterpret_cast<NSError * const*>(src);
  objc_retain(srcInstance);
  if (bridgeNSErrorToErrorType(srcInstance, dest, destType, witness)) {
    if (flags & DynamicCastFlags::TakeOnSuccess)
      objc_release(srcInstance);
    return true;
  }
  return false;
}

static SwiftError *_swift_errorRetain_(SwiftError *error) {
  // For now, SwiftError is always objc-refcounted.
  return (SwiftError*)objc_retain((id)error);
}

extern "C" auto *_swift_errorRetain = _swift_errorRetain_;

SwiftError *swift::swift_errorRetain(SwiftError *error) {
  return _swift_errorRetain(error);
}

static void _swift_errorRelease_(SwiftError *error) {
  // For now, SwiftError is always objc-refcounted.
  return objc_release((id)error);
}

extern "C" auto *_swift_errorRelease = _swift_errorRelease_;

void swift::swift_errorRelease(SwiftError *error) {
  return _swift_errorRelease(error);
}
