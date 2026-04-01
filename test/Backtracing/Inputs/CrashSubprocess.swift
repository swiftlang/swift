#if os(macOS)
internal import Darwin
#elseif canImport(Glibc)
internal import Glibc
#elseif canImport(Musl)
internal import Musl
#endif

func level1() {
  level2()
}

func level2() {
  level3()
}

func level3() {
  level4()
}

func level4() {
  level5()
}

func level5() {
  print("About to crash")
  let ptr = UnsafeMutablePointer<Int>(bitPattern: 4)!
  ptr.pointee = 42
}

@main
struct Crash {
  static func main() {
    print("subprocess stdout is a tty: \(isatty(1))")
    print("subprocess stderr is a tty: \(isatty(2))")
    level1()
  }
}