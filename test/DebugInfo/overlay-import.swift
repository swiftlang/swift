// RUN: %target-swift-frontend -emit-ir -g %s -o - | %FileCheck %s

// REQUIRES: OS=macosx

import Foundation
let n = Notification(name: Notification.Name(rawValue: "test"))
// Test that a skeleton CU for the Foundation module is emitted even though we are only using the overlay!
// CHECK: !DICompileUnit(language: DW_LANG_ObjC, {{.*}}Foundation{{.*}}.pcm", {{.*}}dwoId:
