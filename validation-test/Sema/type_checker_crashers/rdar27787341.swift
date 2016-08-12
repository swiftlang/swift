// RUN: not --crash %target-swift-frontend %s -parse

_ = [1].reduce([:]) { $0[$1] }
