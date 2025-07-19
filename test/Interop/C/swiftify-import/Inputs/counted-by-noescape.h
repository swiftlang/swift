#pragma once

#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __noescape __attribute__((noescape))
#define __lifetimebound __attribute__((lifetimebound))

void simple(int len, int * __counted_by(len) __noescape p);

void swiftAttr(int len, int *p) __attribute__((
    swift_attr("@_SwiftifyImport(.countedBy(pointer: .param(2), count: \"len\"), .nonescaping(pointer: .param(2)), spanAvailability: \"visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4\")")));

void shared(int len, int * __counted_by(len) __noescape p1, int * __counted_by(len) __noescape p2);

void complexExpr(int len, int offset, int * __counted_by(len - offset) __noescape p);

void nullUnspecified(int len, int * __counted_by(len) _Null_unspecified __noescape p);

void nonnull(int len, int * __counted_by(len) _Nonnull __noescape p);

void nullable(int len, int * __counted_by(len) _Nullable p __noescape);

int * __counted_by(len) __noescape returnPointer(int len);

int * __counted_by(len1) returnLifetimeBound(int len1, int len2, int * __counted_by(len2) p __lifetimebound);

void anonymous(int len, int * __counted_by(len) _Nullable __noescape);

void keyword(int len, int * __counted_by(len) _Nullable func __noescape,
    int extension,
    int init,
    int open,
    int var,
    int is,
    int as,
    int in,
    int guard,
    int where
);

void pointerName(int len, int * __counted_by(len) _Nullable pointerName __noescape);

void lenName(int lenName, int size, int * __counted_by(lenName * size) _Nullable p __noescape);

void func(int len, int * __counted_by(len) _Nullable func __noescape);

void *funcRenameKeyword(int len, int * __counted_by(len) _Nullable func __noescape,
    int extension __lifetimebound,
    int init,
    int open,
    int var,
    int is,
    int as,
    int in,
    int guard,
    int where) __attribute__((swift_name("funcRenamed(len:func:extension:init:open:var:is:as:in:guard:where:)")));

void *funcRenameKeywordAnonymous(int len, int * __counted_by(len) _Nullable __noescape,
    int __lifetimebound,
    int,
    int,
    int,
    int,
    int,
    int,
    int,
    int) __attribute__((swift_name("funcRenamedAnon(len:func:extension:init:open:var:is:as:in:guard:where:)")));

void funcRenameClash(int len, int * __counted_by(len) _Nullable func __noescape, int where)
    __attribute__((swift_name("clash(len:func:clash:)")));

void funcRenameClashKeyword(int len, int * __counted_by(len) _Nullable func __noescape, int where)
    __attribute__((swift_name("open(len:func:open:)")));

void funcRenameClashAnonymous(int len, int * __counted_by(len) _Nullable func __noescape, int)
    __attribute__((swift_name("clash2(len:func:clash2:)")));

void funcRenameClashKeywordAnonymous(int len, int * __counted_by(len) _Nullable func __noescape, int)
    __attribute__((swift_name("in(len:func:in:)")));

typedef struct actor_ *actor;
actor _Nonnull keywordType(int len, actor * __counted_by(len) __noescape p, actor _Nonnull p2);
