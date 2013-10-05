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


/// () -> Void
MAKE_BLOCK_SHIM(_TTbbT_T_, void(void));

/// (int64_t) -> Void
MAKE_BLOCK_SHIM(_TTbbSiT_,
                void(int64_t));

/// NSDictionary enumerator
/// (id, id, UnsafePointer<BOOL>) -> Void
MAKE_BLOCK_SHIM(_TTbbTPSo13DynamicLookup_PS__GVSs13UnsafePointerV10ObjectiveC8ObjCBool__T_,
                void(id, id, BOOL *));

/// NSDictionary predicate
/// (id, id, UnsafePointer<BOOL>) -> Void
MAKE_BLOCK_SHIM(_TTbbTPSo13DynamicLookup_PS__GVSs13UnsafePointerV10ObjectiveC8ObjCBool__Sb,
                BOOL(id, id, BOOL *));

/// NSArray enumerator
/// (id, NSUInteger, UnsafePointer<BOOL>) -> Void
MAKE_BLOCK_SHIM(_TTbbTPSo13DynamicLookup_SiGVSs13UnsafePointerV10ObjectiveC8ObjCBool__T_,
                void(id, NSUInteger, BOOL *));

/// NSArray predicate
/// (id, NSUInteger, UnsafePointer<BOOL>) -> Bool
MAKE_BLOCK_SHIM(_TTbbTPSo13DynamicLookup_SiGVSs13UnsafePointerV10ObjectiveC8ObjCBool__Sb,
                BOOL(id, NSUInteger, BOOL *));

/// NSAttributedString enumerator
/// (id, NSRange, UnsafePointer<BOOL>) -> Void  aka attribute string enumerator
MAKE_BLOCK_SHIM(_TTbbTPSo13DynamicLookup_VSC8_NSRangeGVSs13UnsafePointerV10ObjectiveC8ObjCBool__T_,
                void(id, NSRange, BOOL *));

/// NSAttributedString enumerator
/// (NSDictionary, NSRange, UnsafePointer<BOOL>) -> Void
MAKE_BLOCK_SHIM(_TTbbTCSo12NSDictionaryVSC8_NSRangeGVSs13UnsafePointerV10ObjectiveC8ObjCBool__T_,
                void(id, NSRange, BOOL *));

/// NSCalendar enumerator
/// (NSDate, Bool, UnsafePointer<BOOL>) -> Void
MAKE_BLOCK_SHIM(_TTbbTCSo6NSDateSbGVSs13UnsafePointerV10ObjectiveC8ObjCBool__T_,
                void(id, BOOL, BOOL *));

/// NSData enumerator
/// (COpaquePointer, NSRange, UnsafePointer<BOOL>) -> Void
MAKE_BLOCK_SHIM(_TTbbTVSs14COpaquePointerVSC8_NSRangeGVSs13UnsafePointerV10ObjectiveC8ObjCBool__T_,
                void(const void *, NSRange, BOOL *));

/// NSData deallocator
/// (COpaquePointer, NSUInteger) -> Void
MAKE_BLOCK_SHIM(_TTbbTVSs14COpaquePointerSi_T_,
                void(void *, NSUInteger));

/// NSComparator
/// (NSObject, NSObject) -> NSComparisonResult
MAKE_BLOCK_SHIM(_TTbbTCSo8NSObjectS__V10Foundation18NSComparisonResult,
                long(id, id));

/// NSExpression
/// (id, NSArray, NSMutableDictionary) -> id
/* FIXME does this shim need to autorelease the return value?
MAKE_BLOCK_SHIM(_TTbbTPSo13DynamicLookup_CSo7NSArrayCSo19NSMutableDictionary_PS__,
                id(id, NSArray *, NSDictionary *)); */

/// NSFileCoordinator accessor
/// (NSURL) -> Void
MAKE_BLOCK_SHIM(_TTbbCSo5NSURLT_,
                void(NSURL *));

/// NSFileCoordinator accessor
/// (NSURL, NSURL) -> Void
MAKE_BLOCK_SHIM(_TTbbTCSo5NSURLS__T_, 
                void(NSURL *, NSURL *))

/// NSFileCoordinator accessor with completion handler
/// NSFilePresenter accessor with reacquirer
/// ( () -> Void ) -> Void
/// FIXME need to wrap block object parameter differently

/// NSFileHandle handler
/// (NSFileHandle) -> Void
MAKE_BLOCK_SHIM(_TTbbCSo12NSFileHandleT_,
                void(id));

/// NSFileManager error handler
/// (NSURL, NSError) -> Bool
MAKE_BLOCK_SHIM(_TTbbTCSo5NSURLCSo7NSError_Sb,
                BOOL(id, id));

/// NSFilePresenter completion handler
/// NSXPCConnection error handler
/// (NSError) -> Void
MAKE_BLOCK_SHIM(_TTbbCSo7NSErrorT_,
                void(id));

/// NSIndexSet enumerator
/// (NSUInteger, UnsafePointer<BOOL>) -> Void
MAKE_BLOCK_SHIM(_TTbbTSiGVSs13UnsafePointerV10ObjectiveC8ObjCBool__T_,
                void(NSUInteger, BOOL *));

/// NSIndexSet predicate
/// (NSUInteger, UnsafePointer<BOOL>) -> Bool
MAKE_BLOCK_SHIM(_TTbbTSiGVSs13UnsafePointerV10ObjectiveC8ObjCBool__Sb,
                BOOL(NSUInteger, BOOL *));

/// NSNotificationCenter observer
/// (NSNotification) -> Void
MAKE_BLOCK_SHIM(_TTbbCSo14NSNotificationT_,
                void(id));

/// NSRegularExpression enumerator
/// (NSTextCheckingResult, NSMatchingFlags, UnsafePointer<BOOL>) -> Void
MAKE_BLOCK_SHIM(_TTbbTCSo20NSTextCheckingResultVSC15NSMatchingFlagsGVSs13UnsafePointerV10ObjectiveC8ObjCBool__T_,
                void(id, NSMatchingFlags, BOOL *));

/// NSSet enumerator
/// (id, UnsafePointer<BOOL) -> Void
MAKE_BLOCK_SHIM(_TTbbTPSo13DynamicLookup_GVSs13UnsafePointerV10ObjectiveC8ObjCBool__T_,
                void(id, BOOL *));

/// NSSet predicate
/// (id, UnsafePointer<BOOL) -> Bool
MAKE_BLOCK_SHIM(_TTbbTPSo13DynamicLookup_GVSs13UnsafePointerV10ObjectiveC8ObjCBool__Sb,
                BOOL(id, BOOL *));

/// NSSubstring substring enumerator
/// (String, NSRange, NSRange, UnsafePointer<BOOL>) -> Void
/// FIXME String to NSString conversion?

/// NSString line enumerator
/// (NSString, UnsafePointer<BOOL>) -> Void
/// FIXME String to NSString conversion?

/// NSTask termination handler
/// (NSTask) -> Void
MAKE_BLOCK_SHIM(_TTbbCSo6NSTaskT_,
                void(id));

/// NSURLConnection completion handler
/// (NSURLResponse, NSData, NSError) -> Void
MAKE_BLOCK_SHIM(_TTbbTCSo13NSURLResponseCSo6NSDataCSo7NSError_T_,
                void(id, id, id));

/// NSURLSession
/// FIXME lots
