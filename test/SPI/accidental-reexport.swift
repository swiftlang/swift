/// Guard that we don't reexport the SPIs accidentally through the common
/// reexported imports between clang modules.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/Overlayed.swift -I %t -o %t
// RUN: %target-swift-frontend -typecheck -verify %t/Client.swift -I %t
// RUN: %target-swift-frontend -typecheck -verify %t/LowerClient.swift -I %t

//--- module.modulemap
module Overlayed {
    header "Overlayed.h"
}

module Middle {
    header "Middle.h"
    export *
}

module LowerMiddle {
    header "LowerMiddle.h"
    export *
}

//--- Overlayed.h
//--- Overlayed.swift
@_exported import Overlayed

public func funcPublic() {}

@_spi(X)
public func funcSPI() {}

//--- Middle.h
#include <Overlayed.h>

//--- LowerMiddle.h
#include <Middle.h>

//--- Client.swift
@_spi(X) import Middle

funcPublic()
funcSPI() // expected-error {{cannot find 'funcSPI' in scope}}

//--- LowerClient.swift
@_spi(X) import LowerMiddle

funcPublic()
funcSPI() // expected-error {{cannot find 'funcSPI' in scope}}
