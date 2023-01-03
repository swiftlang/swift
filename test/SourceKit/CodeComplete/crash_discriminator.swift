func foo(array: [Int]) {
    _ = array.map { (value) -> Int in
// RUN: %sourcekitd-test -req=complete -pos=%(line+1):20 %s -- %s == -req=complete -pos=%(line+2):16 %s -- %s
        let decoded = 42
        return decoded
    }
}

