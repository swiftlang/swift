// RUN: not %target-swift-frontend %s -parse

let pq = {
    return $0 ?? nil
}
