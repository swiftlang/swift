// RUN: %target-typecheck-verify-swift -solver-scope-threshold=40000

func f(str1: String, str2: String) -> (Any.Type)? {
    return _typeByName(str1)
        ?? _typeByName(str2)
        ?? _typeByName("a.\(str2)")
        ?? _typeByName("b.\(str2)")
        ?? _typeByName("c.\(str2)")
        ?? _typeByName("d.\(str2)")
        ?? _typeByName("e.\(str2)")
        ?? _typeByName("f.\(str2)")
}
