// RUN: not %target-swift-frontend %s -typecheck

let pq = {
    return $0 ?? nil
}
