//===--- BlockShims.mm - Swift closure to ObjC block object shims ---------===//
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
// FIXME <rdar://problem/13020967> Closures should auto-convert to blocks
//
//===----------------------------------------------------------------------===//

#include <Foundation/Foundation.h>

#include "swift/Runtime/Alloc.h"

using namespace swift;

namespace {
  template<typename T> struct block_shim;
  
  template<typename R, typename...A>
  struct block_shim<R(A...)> {
    using block_type = R (^)(A...);
    using code_type = R (*)(A..., HeapObject *);
    
    template<typename...U>
    static void pass(U &&...) {}
    
    template<typename U>
    static void retain_if_id(U const &x) {}
    static void retain_if_id(id x) { [x retain]; }
    
    static block_type shim(code_type code, HeapObject *context) {
      // Manage non-class Swift memory in a way that blocks can understand.
      SwiftRAII contextRAII{context, /*AlreadyRetained=*/true};
      return Block_copy(^R (A...args) {
        // Adjust the context and any id arguments to +1 retain count.
        pass((retain_if_id(args), 0)...);
        return code(args..., swift_retain(*contextRAII));
      });
    }
  };
}

/// MAKE_BLOCK_SHIM(_Tmangled_name, return_type(arg_types...));
#define MAKE_BLOCK_SHIM(MANGLED_NAME, ...) \
  extern "C" block_shim<__VA_ARGS__>::block_type \
  MANGLED_NAME(block_shim<__VA_ARGS__>::code_type code, HeapObject *context) { \
    return block_shim<__VA_ARGS__>::shim(code, context); \
  }

#define CAT2_(A,B) A ## B
#define CAT2(A,B) CAT2_(A,B)
#define CAT3(A,B,C) CAT2(CAT2(A,B), C)
#define CAT4(A,B,C,D) CAT2(CAT2(CAT2(A,B), C), D)

#define FUNC(ARGS, RESULTS) CAT3(CAT3(_TTbXFoCb_, ARGS, _), RESULTS, _)

#define OWNED(T) o##T
#define DIRECT(T) d##T
#define INDIRECT(T) i##T
#define VOID dT_
#define NONE

/// () -> Void
MAKE_BLOCK_SHIM(FUNC(NONE,
                     VOID),
                void(void));

/// (int64_t) -> Void
MAKE_BLOCK_SHIM(FUNC(DIRECT(Si),
                     VOID),
                void(int64_t));

/// NSDictionary enumerator
/// (id, id, UnsafePointer<BOOL>) -> Void
MAKE_BLOCK_SHIM(FUNC(CAT3(OWNED(PSo13DynamicLookup_),
                          OWNED(PS__),
                          DIRECT(GVSs13UnsafePointerV10ObjectiveC8ObjCBool_)),
                     VOID),
                void(id, id, BOOL *));

/// NSDictionary predicate
/// (id, id, UnsafePointer<BOOL>) -> Bool
MAKE_BLOCK_SHIM(FUNC(CAT3(OWNED(PSo13DynamicLookup_),
                          OWNED(PS__),
                          DIRECT(GVSs13UnsafePointerV10ObjectiveC8ObjCBool___)),
                     DIRECT(Sb)),
                BOOL(id, id, BOOL *));

/// NSArray enumerator
/// (id, NSUInteger, UnsafePointer<BOOL>) -> Void
MAKE_BLOCK_SHIM(FUNC(CAT3(OWNED(PSo13DynamicLookup_),
                          DIRECT(Si),
                          DIRECT(GVSs13UnsafePointerV10ObjectiveC8ObjCBool___)),
                     VOID),
                void(id, NSUInteger, BOOL *));

/// NSArray predicate
/// (id, NSUInteger, UnsafePointer<BOOL>) -> Bool
MAKE_BLOCK_SHIM(FUNC(CAT3(OWNED(PSo13DynamicLookup_),
                          DIRECT(Si),
                          DIRECT(GVSs13UnsafePointerV10ObjectiveC8ObjCBool___)),
                     DIRECT(Sb)),
                BOOL(id, NSUInteger, BOOL *));

/// NSAttributedString enumerator
/// (id, NSRange, UnsafePointer<BOOL>) -> Void  aka attribute string enumerator
MAKE_BLOCK_SHIM(FUNC(CAT3(OWNED(PSo13DynamicLookup_),
                          DIRECT(VSC8_NSRange),
                          DIRECT(GVSs13UnsafePointerV10ObjectiveC8ObjCBool___)),
                     VOID),
                void(id, NSRange, BOOL *));

/// NSAttributedString enumerator
/// (NSDictionary, NSRange, UnsafePointer<BOOL>) -> Void
MAKE_BLOCK_SHIM(FUNC(CAT3(OWNED(CSo12NSDictionary),
                          DIRECT(VSC8_NSRange),
                          DIRECT(GVSs13UnsafePointerV10ObjectiveC8ObjCBool___)),
                     VOID),
                void(id, NSRange, BOOL *));

/// NSCalendar enumerator
/// (NSDate, Bool, UnsafePointer<BOOL>) -> Void
MAKE_BLOCK_SHIM(FUNC(CAT3(OWNED(CSo6NSDate),
                          DIRECT(Sb),
                          DIRECT(GVSs13UnsafePointerV10ObjectiveC8ObjCBool___)),
                     VOID),
                void(id, BOOL, BOOL *));

/// NSData enumerator
/// (COpaquePointer, NSRange, UnsafePointer<BOOL>) -> Void
MAKE_BLOCK_SHIM(FUNC(CAT3(DIRECT(VSs14COpaquePointer),
                          DIRECT(VSC8_NSRange),
                          DIRECT(GVSs13UnsafePointerV10ObjectiveC8ObjCBool___)),
                     VOID),
                void(const void *, NSRange, BOOL *));

/// NSData deallocator
/// (COpaquePointer, NSUInteger) -> Void
MAKE_BLOCK_SHIM(FUNC(CAT2(DIRECT(VSs14COpaquePointer),
                          DIRECT(Si)),
                     VOID),
                void(void *, NSUInteger));

/// NSComparator
/// (NSObject, NSObject) -> NSComparisonResult
MAKE_BLOCK_SHIM(FUNC(CAT2(OWNED(CSo8NSObject),
                          OWNED(S_)),
                     DIRECT(V10Foundation18NSComparisonResult)),
                long(id, id));

/// NSExpression
/// (id, NSArray, NSMutableDictionary) -> id
/* FIXME does this shim need to autorelease the return value?
MAKE_BLOCK_SHIM(_TTbbTPSo13DynamicLookup_CSo7NSArrayCSo19NSMutableDictionary_PS__,
                id(id, NSArray *, NSDictionary *)); */

/// NSFileCoordinator accessor
/// (NSURL) -> Void
MAKE_BLOCK_SHIM(FUNC(OWNED(CSo5NSURL),
                     VOID),
                void(NSURL *));

/// NSFileCoordinator accessor
/// (NSURL, NSURL) -> Void
MAKE_BLOCK_SHIM(FUNC(CAT2(OWNED(CSo5NSURL),
                          OWNED(S_)),
                     VOID),
                void(NSURL *, NSURL *))

/// NSFileCoordinator accessor with completion handler
/// NSFilePresenter accessor with reacquirer
/// ( () -> Void ) -> Void
/// FIXME need to wrap block object parameter differently

/// NSFileHandle handler
/// (NSFileHandle) -> Void
MAKE_BLOCK_SHIM(FUNC(OWNED(CSo12NSFileHandle),
                     VOID),
                void(id));

/// NSFileManager error handler
/// (NSURL, NSError) -> Bool
MAKE_BLOCK_SHIM(FUNC(CAT2(OWNED(CSo5NSURL),
                          OWNED(CSo7NSError)),
                     DIRECT(Sb)),
                BOOL(id, id));

/// NSFilePresenter completion handler
/// NSXPCConnection error handler
/// (NSError) -> Void
MAKE_BLOCK_SHIM(FUNC(OWNED(CSo7NSError),
                     VOID),
                void(id));

/// NSIndexSet enumerator
/// (NSUInteger, UnsafePointer<BOOL>) -> Void
MAKE_BLOCK_SHIM(FUNC(CAT2(DIRECT(Si),
                          DIRECT(GVSs13UnsafePointerV10ObjectiveC8ObjCBool___)),
                     VOID),
                void(NSUInteger, BOOL *));

/// NSIndexSet predicate
/// (NSUInteger, UnsafePointer<BOOL>) -> Bool
MAKE_BLOCK_SHIM(FUNC(CAT2(DIRECT(Si),
                          DIRECT(GVSs13UnsafePointerV10ObjectiveC8ObjCBool___)),
                     DIRECT(Sb)),
                BOOL(NSUInteger, BOOL *));

/// NSNotificationCenter observer
/// (NSNotification) -> Void
MAKE_BLOCK_SHIM(FUNC(OWNED(CSo14NSNotification),
                     VOID),
                void(id));

/// NSRegularExpression enumerator
/// (NSTextCheckingResult, NSMatchingFlags, UnsafePointer<BOOL>) -> Void
MAKE_BLOCK_SHIM(FUNC(CAT3(OWNED(CSo20NSTextCheckingResult),
                          OWNED(CSo15NSMatchingFlags),
                          DIRECT(GVSs13UnsafePointerV10ObjectiveC8ObjCBool___)),
                     VOID),
                void(id, NSMatchingFlags, BOOL *));

/// NSSet enumerator
/// (id, UnsafePointer<BOOL>) -> Void
MAKE_BLOCK_SHIM(FUNC(CAT2(OWNED(PSo13DynamicLookup_),
                          DIRECT(GVSs13UnsafePointerV10ObjectiveC8ObjCBool___)),
                     VOID),
                void(id, BOOL *));

/// NSSet predicate
/// (id, UnsafePointer<BOOL>) -> Bool
MAKE_BLOCK_SHIM(FUNC(CAT2(OWNED(PSo13DynamicLookup_),
                          DIRECT(GVSs13UnsafePointerV10ObjectiveC8ObjCBool___)),
                     DIRECT(Sb)),
                BOOL(id, BOOL *));

/// NSSubstring substring enumerator
/// (String, NSRange, NSRange, UnsafePointer<BOOL>) -> Void
/// FIXME String to NSString conversion?

/// NSString line enumerator
/// (NSString, UnsafePointer<BOOL>) -> Void
/// FIXME String to NSString conversion?
MAKE_BLOCK_SHIM(
  FUNC(
    CAT2(OWNED(SS), DIRECT(GVSs13UnsafePointerV10ObjectiveC8ObjCBool_)),
      VOID
  ), void(NSString*, BOOL*))

/// NSTask termination handler
/// (NSTask) -> Void
MAKE_BLOCK_SHIM(FUNC(OWNED(CSo6NSTask),
                     VOID),
                void(id));

/// NSURLConnection completion handler
/// (NSURLResponse, NSData, NSError) -> Void
MAKE_BLOCK_SHIM(FUNC(CAT3(OWNED(CSo13NSURLResponse),
                          OWNED(CSo6NSData),
                          OWNED(CSo7NSError)),
                     VOID),
                void(id, id, id));

/// enumerateLinguisticTagsInRange callback
/// (NSString, NSRange, NSRange, UnsafePointer<BOOL>) -> Void
MAKE_BLOCK_SHIM(FUNC(CAT4(OWNED(SS),
                          DIRECT(VSC8_NSRange),
                          DIRECT(S_),
                          DIRECT(GVSs13UnsafePointerV10ObjectiveC8ObjCBool_)),
                     VOID),
                void(NSString*, NSRange, NSRange, BOOL*));

/// NSURLSession
/// FIXME lots
