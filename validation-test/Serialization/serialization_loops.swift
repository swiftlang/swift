// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -import-objc-header %S/Inputs/serialization_loops.h -o %t %S/Inputs/serialization_loops_helper.swift
// RUN: %target-swift-frontend -emit-module -import-objc-header %S/Inputs/serialization_loops.h -I %t -o %t %S/Inputs/serialization_loops_helper2.swift
// RUN: %target-swift-frontend -emit-sil -import-objc-header %S/Inputs/serialization_loops.h -I %t -o /dev/null %s

// REQUIRES: objc_interop

// This test case is a model of rdar://problem/22364953.

import serialization_loops_helper
import serialization_loops_helper2

extension Sub {}
