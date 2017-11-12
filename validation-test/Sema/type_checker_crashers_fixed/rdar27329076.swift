// RUN: not %target-swift-frontend %s -typecheck

_ = try [ { return .D($0[0]) } ]
