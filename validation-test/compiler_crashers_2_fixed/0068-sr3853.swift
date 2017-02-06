// RUN: rm -rf %t && mkdir %t
// RUN: %target-swift-frontend -emit-module %s -DLIBRARY -I %S/Inputs/0068-sr3853/ -o %t/Lib.swiftmodule
// RUN: %target-swift-frontend -emit-sil -primary-file %s %S/Inputs/0068-sr3853/other.swift -I %S/Inputs/0068-sr3853/ -I %t -module-name main -DVALID

// Try again in an error configuration to make sure we don't crash.
// RUN: %target-swift-frontend -emit-sil -primary-file %s %S/Inputs/0068-sr3853/other.swift -I %S/Inputs/0068-sr3853/ -I %t -module-name main

// REQUIRES: objc_interop

#if LIBRARY

import BaseLib

public class GrandSub: Sub {}

#else

import Lib

func foo(object: GrandSub) { }

#endif