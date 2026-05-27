#pragma once

#if __has_feature(nullability)
_Pragma("clang assume_nonnull begin");
_Pragma("clang diagnostic push");
_Pragma("clang diagnostic ignored \"-Wnullability-extension\"");
#endif

#define _STRUCT_PAYLOAD                                                        \
  int get() const { return payload; }                                          \
  void set(int x) { payload = x; }                                             \
  int payload = 0

#define _NO_ATTR(Name, MakeAttrs)                                              \
  {                                                                            \
    _STRUCT_PAYLOAD;                                                           \
  };                                                                           \
  MakeAttrs inline Name *make##Name() { return new Name{}; }                   \
  struct Name

/// Struct without any reference type annotations
#define NO_ATTR(Name) _NO_ATTR(Name, )

/// Struct without reference type annotations, that is returned at +1 ref count
#define NO_ATTR_SHARED(Name)                                                   \
  _NO_ATTR(Name, __attribute__((swift_attr("returns_retained"))))

/// Struct with immortal reference type annotations
#define IMMORTAL(Name)                                                         \
  {                                                                            \
    _STRUCT_PAYLOAD;                                                           \
  }                                                                            \
  __attribute__((swift_attr("import_reference")))                              \
  __attribute__((swift_attr("retain:immortal")))                               \
  __attribute__((swift_attr("release:immortal")));                             \
  inline Name *make##Name() {                                                  \
    static Name t{};                                                           \
    return &t;                                                                 \
  }                                                                            \
  struct Name

#define _SHARED(Name, RetainRelease)                                           \
  {                                                                            \
    int refcount = 1;                                                          \
    _STRUCT_PAYLOAD;                                                           \
  }                                                                            \
  __attribute__((swift_attr("import_reference")))                              \
  __attribute__((swift_attr("retain:retain" #RetainRelease)))                  \
  __attribute__((swift_attr("release:release" #RetainRelease)));               \
  __attribute__((swift_attr("returns_retained"))) inline Name *make##Name() {  \
    return new Name{};                                                         \
  }                                                                            \
  struct Name

/// A struct with shared reference annotations and retain + release functions.
/// In the release function, we omit the deletion operation and deliberately
/// leak the object because getting deletion right involves extra scaffolding to
/// make sure t points to the beginning of a derived object when are in a base
/// class release function, typically done using CRTP or by embedding memory
/// layout info into the base class.
#define SHARED(Name)                                                           \
  _SHARED(Name, Name);                                                         \
  inline void retain##Name(Name *t) { ++t->refcount; }                         \
  inline void release##Name(Name *t) {                                         \
    if (--t->refcount <= 0)                                                    \
      (void)"DELETION PLACEHOLDER";                                            \
  }                                                                            \
  struct Name

/// Struct with shared reference annotations, reusing a base's retain/release
/// functions
#define SHARED_USING_BASE(Name, Base) _SHARED(Name, Base)

// MARK: Simple: simple record types without inheritance

struct SimpleValue NO_ATTR(SimpleValue);
struct SimpleShared SHARED(SimpleShared);
struct SimpleImmortal IMMORTAL(SimpleImmortal);

// MARK: SingleShared: single inheritance with a shared refence base

struct SingleShared_Base SHARED(SingleShared_Base);

struct SingleShared_Shared : SingleShared_Base SHARED(SingleShared_Shared);
struct SingleShared_NoAttr
    : SingleShared_Base NO_ATTR_SHARED(SingleShared_NoAttr);

struct SingleShared_Shared_Final final
    : SingleShared_Base SHARED(SingleShared_Shared_Final);
struct SingleShared_NoAttr_Final final
    : SingleShared_Base NO_ATTR_SHARED(SingleShared_NoAttr_Final);

struct SingleShared_Shared_Shared
    : SingleShared_Shared SHARED(SingleShared_Shared_Shared);
// BUG: should be allowed, we're just singly inheriting from a shared ref
// expected-warning@+1 {{unable to infer SWIFT_SHARED_REFERENCE}}
struct SingleShared_Shared_NoAttr
    : SingleShared_Shared NO_ATTR(SingleShared_Shared_NoAttr);
struct SingleShared_NoAttr_Shared
    : SingleShared_NoAttr SHARED(SingleShared_NoAttr_Shared);
struct SingleShared_NoAttr_NoAttr
    : SingleShared_NoAttr NO_ATTR_SHARED(SingleShared_NoAttr_NoAttr);

// MARK: SingleImmortal: single inheritance with an immortal refence base

struct SingleImmortal_Base IMMORTAL(SingleImmortal_Base);

struct SingleImmortal_Immort
    : SingleImmortal_Base IMMORTAL(SingleImmortal_Immort);
struct SingleImmortal_NoAttr
    : SingleImmortal_Base NO_ATTR(SingleImmortal_NoAttr);

struct SingleImmortal_Immort_Final final
    : SingleImmortal_Base IMMORTAL(SingleImmortal_Immort_Final);
struct SingleImmortal_NoAttr_Final final
    : SingleImmortal_Base NO_ATTR(SingleImmortal_NoAttr_Final);

struct SingleImmortal_Immort_Immort
    : SingleImmortal_Immort IMMORTAL(SingleImmortal_Immort_Immort);
struct SingleImmortal_Immort_NoAttr
    : SingleImmortal_Immort NO_ATTR(SingleImmortal_Immort_NoAttr);
struct SingleImmortal_NoAttr_Immort
    : SingleImmortal_NoAttr IMMORTAL(SingleImmortal_NoAttr_Immort);
struct SingleImmortal_NoAttr_NoAttr
    : SingleImmortal_NoAttr NO_ATTR(SingleImmortal_NoAttr_NoAttr);

// MARK: OverloadShared: single inheritance with overloaded retain/release ops

struct OverloadShared_Base SHARED(OverloadShared_Base);

struct OverloadShared_Shared
    : OverloadShared_Base SHARED_USING_BASE(OverloadShared_Shared,
                                            OverloadShared_Base);
struct OverloadShared_Shared_Shared
    : OverloadShared_Shared SHARED_USING_BASE(OverloadShared_Shared_Shared,
                                              OverloadShared_Base);
struct OverloadShared_Shared_NoAttr
    : OverloadShared_Shared SHARED_USING_BASE(OverloadShared_Shared_NoAttr,
                                              OverloadShared_Base);

// MARK: OneShared: multiple inheritance with one shared reference base

struct OneShared_R SHARED(OneShared_R);  // the shared reference base
struct OneShared_U NO_ATTR(OneShared_U); // an unannotated value base
struct OneShared_DR : OneShared_R        // derives the shared ref base
                          NO_ATTR_SHARED(OneShared_DR);

struct OneShared_RU_Shared : OneShared_R,
                             OneShared_U SHARED(OneShared_RU_Shared);
struct OneShared_UR_Shared : OneShared_U,
                             OneShared_R SHARED(OneShared_UR_Shared);
struct OneShared_RU_NoAttr : OneShared_R,
                             OneShared_U NO_ATTR_SHARED(OneShared_RU_NoAttr);
struct OneShared_UR_NoAttr : OneShared_U,
                             OneShared_R NO_ATTR_SHARED(OneShared_UR_NoAttr);
struct OneShared_DRU_Shared : OneShared_DR,
                              OneShared_U SHARED(OneShared_DRU_Shared);
struct OneShared_UDR_Shared : OneShared_U,
                              OneShared_DR SHARED(OneShared_UDR_Shared);
struct OneShared_DRU_NoAttr : OneShared_DR,
                              OneShared_U NO_ATTR_SHARED(OneShared_DRU_NoAttr);
struct OneShared_UDR_NoAttr : OneShared_U,
                              OneShared_DR NO_ATTR_SHARED(OneShared_UDR_NoAttr);

// MARK: TwoShared: inheriting two shared reference bases

struct TwoShared_A SHARED(TwoShared_A);
struct TwoShared_B SHARED(TwoShared_B);

// expected-warning@+1 {{unable to infer SWIFT_SHARED_REFERENCE}}
struct TwoShared_NoAttr : TwoShared_A, TwoShared_B NO_ATTR(TwoShared_NoAttr);
struct TwoShared_Shared : TwoShared_A, TwoShared_B SHARED(TwoShared_Shared);
struct TwoShared_Shared_UsingA
    : TwoShared_A,
      TwoShared_B SHARED_USING_BASE(TwoShared_Shared_UsingA, TwoShared_A);
struct TwoShared_Shared_UsingB
    : TwoShared_A,
      TwoShared_B SHARED_USING_BASE(TwoShared_Shared_UsingB, TwoShared_B);

// MARK: DiamondRef: inheriting same ref base twice due to diamond inheritance

struct DiamondRef_Base SHARED(DiamondRef_Base);
struct DiamondRef_A : DiamondRef_Base NO_ATTR_SHARED(DiamondRef_A);
struct DiamondRef_B : DiamondRef_Base NO_ATTR_SHARED(DiamondRef_B);
struct DiamondRef_VA : virtual DiamondRef_Base NO_ATTR_SHARED(DiamondRef_VA);
struct DiamondRef_VB : virtual DiamondRef_Base NO_ATTR_SHARED(DiamondRef_VB);

// expected-warning@+1 {{unable to infer SWIFT_SHARED_REFERENCE}}
struct DiamondRef_NoAttr : DiamondRef_A,
                           DiamondRef_B NO_ATTR(DiamondRef_NoAttr);
struct DiamondRef_Shared : DiamondRef_A, DiamondRef_B SHARED(DiamondRef_Shared);

struct DiamondRef_VV_NoAttr
    : DiamondRef_VA,
      DiamondRef_VB NO_ATTR_SHARED(DiamondRef_VV_NoAttr);
struct DiamondRef_VV_Shared : DiamondRef_VA,
                              DiamondRef_VB SHARED(DiamondRef_VV_Shared);
// expected-warning@+1 {{unable to infer SWIFT_SHARED_REFERENCE}}
struct DiamondRef_XV_NoAttr : DiamondRef_A,
                              DiamondRef_VB NO_ATTR(DiamondRef_XV_NoAttr);
// expected-warning@+1 {{unable to infer SWIFT_SHARED_REFERENCE}}
struct DiamondRef_VX_NoAttr : DiamondRef_VA,
                              DiamondRef_B NO_ATTR(DiamondRef_VX_NoAttr);
struct DiamondRef_XV_Shared : DiamondRef_A,
                              DiamondRef_VB SHARED(DiamondRef_XV_Shared);
struct DiamondRef_VX_Shared : DiamondRef_VA,
                              DiamondRef_B SHARED(DiamondRef_VX_Shared);

// MARK: DiamondNoRef: diamond inheritance where the repeated base is not a ref
struct DiamondNoRef_Base NO_ATTR(DiamondNoRef_Base);
struct DiamondNoRef_A : DiamondNoRef_Base NO_ATTR(DiamondNoRef_A);
struct DiamondNoRef_B : DiamondNoRef_Base NO_ATTR(DiamondNoRef_B);
struct DiamondNoRef_RA : DiamondNoRef_Base SHARED(DiamondNoRef_RA);
struct DiamondNoRef_RB : DiamondNoRef_Base SHARED(DiamondNoRef_RB);

struct DiamondNoRef_ARB : DiamondNoRef_A,
                          DiamondNoRef_RB NO_ATTR_SHARED(DiamondNoRef_ARB);
struct DiamondNoRef_RAB : DiamondNoRef_RA,
                          DiamondNoRef_B NO_ATTR_SHARED(DiamondNoRef_RAB);
// expected-warning@+1 {{unable to infer SWIFT_SHARED_REFERENCE}}
struct DiamondNoRef_RARB : DiamondNoRef_RA,
                           DiamondNoRef_RB NO_ATTR(DiamondNoRef_RARB);

#if __has_feature(nullability)
_Pragma("clang diagnostic pop");
_Pragma("clang assume_nonnull end");
#endif
