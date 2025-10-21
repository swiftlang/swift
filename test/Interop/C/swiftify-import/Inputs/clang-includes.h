#pragma once

#include "TransitiveModules/module-a.h"
#include "TransitiveModules/module-b.h"
#include "TransitiveModules/module-c.h"
#include "TransitiveModules/module-d.h"
#include "TransitiveModules/module-e.h"

#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __noescape __attribute__((noescape))

void basic_include(const a_t *__counted_by(len) p __noescape, a_t len);

void non_exported_include(const b_t *__counted_by(len) p __noescape, b_t len);

void submodule_include(const c_t *__counted_by(len) p __noescape, c_t len);

void explicit_submodule_include(const d_t *__counted_by(len) p __noescape, d_t len);

void deep_submodule_noexport(const e_t *__counted_by(len) p __noescape, e_t len);
