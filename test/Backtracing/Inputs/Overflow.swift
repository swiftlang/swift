var x: UInt = 0

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
  print("About to overflow")

  x -= 1
}

@main
struct Overflow {
  static func main() {
    level1()
  }
}
