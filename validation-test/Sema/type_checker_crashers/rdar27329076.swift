// RUN: not --crash %target-swift-frontend %s -parse

_ = try [ { return .D($0[0]) } ]
