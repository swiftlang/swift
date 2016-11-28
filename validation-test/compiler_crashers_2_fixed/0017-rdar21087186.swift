// RUN: not %target-swift-frontend %s -typecheck

protocol Empty {}

extension Empty {
    protocol ThisCrashesSwift {}
}
