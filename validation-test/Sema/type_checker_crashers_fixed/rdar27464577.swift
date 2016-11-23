// RUN: not %target-swift-frontend %s -parse

_ = [1].map { v -> T in return v as? Int }
