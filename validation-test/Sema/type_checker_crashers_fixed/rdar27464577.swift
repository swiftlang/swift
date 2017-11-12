// RUN: not %target-swift-frontend %s -typecheck

_ = [1].map { v -> T in return v as? Int }
