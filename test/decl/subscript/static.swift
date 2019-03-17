// RUN: %target-typecheck-verify-swift

// FIXME: Write proper tests (this file is mainly suitable for interactive testing)

struct MyStruct {
    static subscript(_ i: Int) -> String {
        return "success"
    }
}

print(MyStruct.self[0])
print(MyStruct[0])
