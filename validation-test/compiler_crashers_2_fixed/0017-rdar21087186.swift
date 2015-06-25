// RUN: not %target-swift-frontend %s -parse

protocol Empty {}

extension Empty {
    protocol ThisCrashesSwift {}
}
