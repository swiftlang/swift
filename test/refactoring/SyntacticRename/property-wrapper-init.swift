public struct Outer {
    @propertyWrapper
    public struct InnerWrapper<T> {
        public var wrappedValue: T
        public /*init:def*/init(initialValue: T) {
            self.wrappedValue = initialValue
        }
        public /*body:def*/init(first: Int, body: () -> T) {
            self.wrappedValue = body()
        }
    }
}

var globalInt: Int { return 17 }
public struct HasWrappers {
    @Outer.InnerWrapper
    public var x: Int = globalInt
    
    @Outer . /*body:call*/InnerWrapper(first: 20, body: { globalInt })
    public var y: Int
    
    @Outer . /*body:call*/InnerWrapper(first: 10, body: {
        struct Inner {
            @Outer . /*init:call*/InnerWrapper(initialValue: globalInt)
            var x: Int
        }
        return Inner().x + globalInt
    })
    public var z: Int
}

func uses() {
    _ = Outer . /*body:call*/InnerWrapper(first: 42, body: { 45 })
    _ = Outer . /*init:call*/InnerWrapper(initialValue: 0)
}


// REQUIRES: swift_swift_parser
// RUN: %empty-directory(%t.result)
// RUN: %empty-directory(%t.ranges)
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="init" -is-function-like -old-name "init(initialValue:)" >> %t.ranges/property-wrapper-init.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="body" -is-function-like -old-name "init(first:body:)" >> %t.ranges/property-wrapper-body.swift
// RUN: diff -u %S/Outputs/property-wrapper-init/init.swift.expected %t.ranges/property-wrapper-init.swift
// RUN: diff -u %S/Outputs/property-wrapper-init/body.swift.expected %t.ranges/property-wrapper-body.swift
