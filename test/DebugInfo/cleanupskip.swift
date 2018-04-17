// RUN: %target-swift-frontend -emit-ir -g %s -o - -O -disable-llvm-optzns | %FileCheck %s
// REQUIRES: objc_interop
import Foundation

public class NSCoder : NSObject {}

public class AClass : NSObject {
  // Ensure that the call to the type metadata accessor has a line number.
  // CHECK: call swiftcc %swift.metadata_response @"$S11cleanupskip7NSCoderCMa"
  // CHECK-SAME:              !dbg ![[LINEZ:[0-9]+]]
  // CHECK: ![[LINEZ]] = {{.*}}line: 0
  public required init?(coder aDecoder: NSCoder) {
        return nil
    }
}

