// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -I %t/Inputs -cxx-interoperability-mode=default -module-name main %t/program.swift
// RUN: %target-swift-frontend -typecheck -verify -I %t/Inputs -cxx-interoperability-mode=off -module-name main %t/program.swift
// REQUIRES: objc_interop

//--- Inputs/module.modulemap
module TheModule {
    header "the-header.h"
}

//--- Inputs/the-header.h
#pragma once

#define CF_SWIFT_NAME(_name) __attribute__((swift_name(#_name)))

#define __CF_OPTIONS_ATTRIBUTES __attribute__((flag_enum,enum_extensibility(open)))
#if (__cplusplus)
#define CF_OPTIONS(_type, _name) __attribute__((availability(swift,unavailable))) _type _name; enum __CF_OPTIONS_ATTRIBUTES : _name
#else
#define CF_OPTIONS(_type, _name) enum __CF_OPTIONS_ATTRIBUTES _name : _type _name; enum _name : _type
#endif

typedef CF_OPTIONS(unsigned, TheFlags) {
  TheFlagsFoo = (1 << 1),
  TheFlagsBar = (1 << 2)
} CF_SWIFT_NAME(The.Flags);

typedef TheFlags DaFlags;

struct TheContext {
  TheFlags flags;
} CF_SWIFT_NAME(The);

//--- program.swift
import TheModule

func f(_ _: DaFlags) {}
