struct Foo<T> {
  var value: T
}

func crash() {
  print("I'm going to crash here")
  let ptr = UnsafeMutablePointer<Int>(bitPattern: 4)!
  ptr.pointee = 42
}

@main
struct CrashWithThunk {
  static func main() {
    let foo = Foo(value: crash)

    foo.value()
  }
}
