@propertyWrapper
struct Wrapper<T> {
    var wrappedValue: T
    /*split:def*/init(initialValue: T, fieldName: String, special: Bool = false) {
        wrappedValue = initialValue
    }
}
let /*someValue:def*/someValue = "some"
struct User {
    @/*split:call*/Wrapper(fieldName: "bar")
    var bar = 10

    @/*split:call*/Wrapper(fieldName: {
        return /*someValue*/someValue
    }(), special: true)
    var complex: Int = {
        return /*someValue*/someValue.starts(with: "b") ? 43 : 0
    }()
}


// REQUIRES: swift_swift_parser
// RUN: %empty-directory(%t.ranges)
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="someValue" -old-name "someValue" >> %t.ranges/property-wrapper-split-someValue.swift
// RUN: diff -u %S/Outputs/property-wrapper-split/someValue.swift.expected %t.ranges/property-wrapper-split-someValue.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="split" -is-function-like -old-name "init(initialValue:fieldName:special:)" >> %t.ranges/property-wrapper-split-split.swift
// RUN: diff -u %S/Outputs/property-wrapper-split/split.swift.expected %t.ranges/property-wrapper-split-split.swift
