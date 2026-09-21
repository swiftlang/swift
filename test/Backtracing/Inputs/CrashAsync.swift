@available(SwiftStdlib 5.1, *)
func crash() {
  let ptr = UnsafeMutablePointer<Int>(bitPattern: 4)!
  ptr.pointee = 42
}

@available(SwiftStdlib 5.1, *)
func level(_ n: Int) async {
  if n < 5 {
    await level(n + 1)
  } else {
    crash()
  }
}

@available(SwiftStdlib 5.1, *)
@main
struct CrashAsync {
  static func main() async {
    await level(1)
  }
}
