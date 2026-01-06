// RUN: not %target-swift-frontend %s -typecheck

enum E : Equatable {
    case c(Int)
    case c(String)
}
