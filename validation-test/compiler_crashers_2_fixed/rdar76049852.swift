// RUN: not %target-swift-frontend -typecheck %s

struct MyStruct {}
protocol MyProtocol {}

func foo(bytes: [MyStruct]) {
  bytes.withUnsafeBufferPointer { a in
    extension MyProtocol {
      var bytes: MyStruct {
        fatalError()
      }
    }
  }
}
