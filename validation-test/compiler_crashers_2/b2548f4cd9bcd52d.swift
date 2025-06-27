// {"signature":"isEscaping(swift::Type)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a->String{ b}
         func b(c : (sending String
