// RUN: %target-swift-frontend -emit-sil -primary-file %s -sil-verify-all 

// REQUIRES: VENDOR=apple

import OSLogTestHelper

// Ensure no SIL verifier error
public class ActivityHeartbeat {
  private var lastActivity: Int

  init(now: @escaping () -> Int) {
    self.lastActivity = now()
    _osLogTestHelper("ActivityHeartbeat: initialized \(self.lastActivity)")
  }
}

