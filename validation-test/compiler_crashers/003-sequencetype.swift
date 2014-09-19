// RUN: %swift %s -parse -verify
// Test case submitted to project by https://github.com/practicalswift (practicalswift)

struct c<d : SequenceType> {
    var b: d
}

func a<d>() -> [c<d>] {
    return []
}
