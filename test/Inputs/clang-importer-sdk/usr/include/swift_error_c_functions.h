/* -*- ObjC -*- */
@import Foundation;
@import CoreFoundation;
#include <MacTypes.h>
#include <stdbool.h>

// --- swift_error conventions on C functions ---

// nonnull_error: _Bool return preserved.
_Bool c_error_bound(NSError **err)
  __attribute__((swift_error(nonnull_error)));

// nonnull_error: float return preserved.
float c_error_bounce(NSError **err)
  __attribute__((swift_error(nonnull_error)));

// nonnull_error: void return.
void c_error_flounce(NSError **err)
  __attribute__((swift_error(nonnull_error)));

// zero_result: int return → ZeroPreservedResult, return preserved.
int c_error_ounce(NSError **err)
  __attribute__((swift_error(zero_result)));

// nonzero_result: int return → Void.
int c_error_once(NSError **err)
  __attribute__((swift_error(nonzero_result)));

// zero_result: _Bool return → ZeroResult, Void.
_Bool c_error_sconce(NSError **err)
  __attribute__((swift_error(zero_result)));

// nonzero_result: _Bool return → Void.
_Bool c_error_scotch(NSError **err)
  __attribute__((swift_error(nonzero_result)));

// none: no throwing variant.
_Bool c_error_scout(NSError **err)
  __attribute__((swift_error(none)));

// --- Boolean type variants for zero_result ---
// All boolean-like types should produce ZeroResult (throwing returns Void).

// bool (stdbool.h macro for _Bool).
bool c_error_zero_bool(NSError **err)
  __attribute__((swift_error(zero_result)));

// BOOL (ObjC typedef).
BOOL c_error_zero_BOOL(NSError **err)
  __attribute__((swift_error(zero_result)));

// Boolean (Carbon/MacTypes.h typedef to unsigned char).
Boolean c_error_zero_Boolean(NSError **err)
  __attribute__((swift_error(zero_result)));

// --- Boolean type variants for nonzero_result ---

bool c_error_nonzero_bool(NSError **err)
  __attribute__((swift_error(nonzero_result)));

BOOL c_error_nonzero_BOOL(NSError **err)
  __attribute__((swift_error(nonzero_result)));

Boolean c_error_nonzero_Boolean(NSError **err)
  __attribute__((swift_error(nonzero_result)));

// --- null_result with NSError** ---

void * _Nullable c_error_ns_null(NSError **err)
  __attribute__((swift_error(null_result)));

// --- Additional cases ---

// null_result: nullable pointer return with CFErrorRef* CF_RETURNS_RETAINED.
void * _Nullable c_error_cf_null(CFErrorRef *err CF_RETURNS_RETAINED)
  __attribute__((swift_error(null_result)));

// nonnull_error: void return with CFErrorRef* CF_RETURNS_NOT_RETAINED.
void c_error_cf_nonnull(CFErrorRef *err CF_RETURNS_NOT_RETAINED)
  __attribute__((swift_error(nonnull_error)));

// Trailing block parameter after error.
_Bool c_error_trailing_block(NSError **err, void (^callback)(void))
  __attribute__((swift_error(zero_result)));

// Explicit argument labels with a block parameter after the error parameter.
_Bool c_error_labeled_trailing_block(NSError **err, void (^callback)(void))
  __attribute__((swift_name("labeledTrailingBlock(err:callback:)")))
  __attribute__((swift_error(zero_result)));

// Block parameter before error (error is last).
_Bool c_error_block_before(int x, void (^callback)(void), NSError **err)
  __attribute__((swift_error(zero_result)));

// Multiple trailing blocks after error.
_Bool c_error_multi_trailing_blocks(NSError **err,
    void (^first)(void), void (^second)(void))
  __attribute__((swift_error(zero_result)));

// Multiple blocks before error (error is last).
_Bool c_error_multi_blocks_before(void (^first)(void),
    void (^second)(void), NSError **err)
  __attribute__((swift_error(zero_result)));

// Blocks on both sides of error.
_Bool c_error_blocks_both_sides(void (^before)(void),
    NSError **err, void (^after)(void))
  __attribute__((swift_error(zero_result)));

// Multiple parameters before error.
_Bool c_error_multi_param(int x, int y, NSError **err)
  __attribute__((swift_error(zero_result)));

// A lifetime annotation on the error parameter, which the throwing variant
// omits from its parameter list.
void * _Nullable c_error_lifetimebound(NSError **err __attribute__((lifetimebound)));

// --- Negative cases ---

// CFErrorRef* without ownership annotation.
_Bool c_error_cf_bare(CFErrorRef *err)
  __attribute__((swift_error(zero_result)));

// No swift_error attribute with CFErrorRef* (no ownership).
_Bool c_no_attr_cf(int x, CFErrorRef *err);

// --- Default heuristics (no swift_error attribute) ---

// bool + NSError** → ZeroResult.
bool c_default_bool_nserror(NSError **err);

// _Nullable pointer + NSError** → NilResult.
void * _Nullable c_default_nullable_nserror(NSError **err);

// Unannotated pointer + NSError** → NilResult (IUO default).
void *c_default_unannotated_ptr_nserror(NSError **err);

// bool + CFErrorRef* with ownership → ZeroResult.
bool c_default_bool_cferror(CFErrorRef *err CF_RETURNS_RETAINED);

// _Bool + int param + NSError** → ZeroResult via default heuristic.
_Bool c_no_attr(int x, NSError **err);

// --- Default heuristic negative cases ---

// int return → no default heuristic (not bool, not pointer).
int c_default_int_nserror(NSError **err);

// CFErrorRef* without ownership → still blocked.
bool c_default_bool_cferror_bare(CFErrorRef *err);

// A union bridged to NSError is not CFError, which Clang identifies by a
// struct tag, so no error convention applies.
typedef union __attribute__((objc_bridge(NSError))) __CFErrorUnion *CFErrorUnionRef;

bool c_default_bool_error_union(CFErrorUnionRef *err CF_RETURNS_RETAINED);

// A CF type bridged to NSError that is not CFError itself.
typedef struct __attribute__((objc_bridge_mutable(NSError))) __CFCustomError *CFCustomErrorRef;

bool c_default_bool_custom_cferror(CFCustomErrorRef *err CF_RETURNS_RETAINED);

// The throwing variant drops the error parameter, giving it the same name as
// another declaration.
_Bool c_collide_source(int x, NSError **err)
  __attribute__((swift_name("collide(x:err:)")))
  __attribute__((swift_error(zero_result)));

void c_collide_target(int x)
  __attribute__((swift_name("collide(x:)")));

// swift_error(none) overrides default heuristic.
bool c_default_override_none(NSError **err)
  __attribute__((swift_error(none)));

// _Nonnull pointer + NSError** → no NilResult (non-optional return).
void * _Nonnull c_default_nonnull_nserror(NSError **err);

// void return + NSError** → no default heuristic.
void c_default_void_nserror(NSError **err);

// --- swift_error attribute / return type mismatches accepted by Clang ---
// Clang rejects most return-type mismatches at parse time (e.g.,
// zero_result/nonzero_result require an integral return). The remaining case
// Clang allows but the Swift importer must still reject is null_result on a
// non-optional pointer return: classifyFunctionErrorHandling returns nullopt
// because OTK_None blocks the NilResult convention.
void * _Nonnull c_error_null_nonnull(NSError **err)
  __attribute__((swift_error(null_result)));

// --- Imported as a member of a type ---
// swift_name can map a C function onto a type, turning one parameter into
// 'self'. The error parameter is preserved for these; the throwing variant
// covers module-scope functions only.

@interface CErrorWidget : NSObject
@end

// The error convention on ObjC methods covers NSError** only, so a CFErrorRef*
// out-parameter stays an ordinary parameter even with the attribute.
@interface CErrorObjCWidget : NSObject
- (BOOL)doIt:(CFErrorRef *)err __attribute__((swift_error(zero_result)));
@end

_Bool c_member_zero_result(CErrorWidget *widget, NSError **err)
  __attribute__((swift_name("CErrorWidget.doIt(self:error:)")))
  __attribute__((swift_error(zero_result)));

struct CErrorPoint {
  double x, y;
};

_Bool c_member_struct_zero_result(struct CErrorPoint *point, NSError **err)
  __attribute__((swift_name("CErrorPoint.doIt(self:error:)")))
  __attribute__((swift_error(zero_result)));
