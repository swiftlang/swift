#pragma once

struct CGImageBlock {
    int width;
    int height;
};

typedef struct CGImageBlock *CGImageBlockRef;

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:Retain")))
__attribute__((swift_attr("release:Release"))) FRT {};

static inline void Retain(struct FRT *x) {}
static inline void Release(struct FRT *x) {}
