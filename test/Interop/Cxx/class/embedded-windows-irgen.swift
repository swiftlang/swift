// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-ir -I %t -cxx-interoperability-mode=default -enable-experimental-feature Embedded -parse-as-library %t/test.swift -o /dev/null

// REQUIRES: OS=windows-msvc
// REQUIRES: embedded_stdlib
// REQUIRES: swift_feature_Embedded

//--- module.modulemap
module Record {
    header "record.h"
    requires cplusplus
}

//--- record.h
#pragma once

struct Record {
  long long value = 10;
};

//--- test.swift
import Record

public func make() -> Int64 { Record().value }
