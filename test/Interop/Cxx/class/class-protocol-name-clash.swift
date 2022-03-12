// RUN: %target-swift-frontend %s -c -enable-cxx-interop -I %S/Inputs
//
// REQUIRES: objc_interop

import ClassProtocolNameClash

class Subclass : TheClashingName {}
