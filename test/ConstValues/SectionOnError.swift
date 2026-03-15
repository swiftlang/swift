// RUN: %target-swift-frontend -emit-ir %s -verify
// Ensure that we do not diagnose an inability to statically-initialize globals when they contain a prior (in the compilation pipeline) error
@section("__TEXT,__mysection") var tooBig: Int = 10000000000000000000000000000000000000 //expected-error {{integer literal '10000000000000000000000000000000000000' overflows when stored into 'Int'}}
