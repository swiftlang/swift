// RUN: %target-swift-frontend -disable-availability-checking -emit-sil -verify %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation
import os.log

struct Log {
  static let attachmentLedgerTopic = Logger(
    subsystem: "com.xxx.yyy.zzz",
    category: "ccc")
}

// CHECK-LABEL: sil @logWriter : {{.*}} {
// CHECK-NOT: $s2os18OSLogInterpolationV13appendLiteralyySSF
// CHECK-LABEL: } // end sil function 'logWriter'
@_silgen_name("logWriter")
public func logWriter(_ attachmentID: UUID) {
  Log.attachmentLedgerTopic.info("aaa: \(attachmentID)")
}
