// RUN: %target-swift-frontend -emit-ir -primary-file %s

// REQUIRES: CPU=arm64_32 || CPU=armv7k

public class MyClass {
    let x: Int64
    init(x: Int64) {
        self.x = x
    }
}

public struct MyError: Error {
    let x: MyClass
}

@inline(never)
public func foo(f: () throws(MyError) -> Int64) throws(MyError) -> Int64 {
    return try f()
}

public func bar(f: () throws(MyError) -> Int64) -> Int64 {
    do {
        return try foo(f: f)
    } catch {
        return error.x.x
    }
}
