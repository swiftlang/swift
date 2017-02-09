// RUN: %target-swift-frontend %s -module-name Test -sil-serialize-all -emit-module -emit-module-path - -o /dev/null | %target-sil-opt -enable-sil-verify-all -module-name="Test" | %FileCheck %s

// Check that default witness tables are properly deserialized.
// rdar://problem/29173229

// CHECK: import Swift
public class MyType {
}

public protocol MyProto {
}

