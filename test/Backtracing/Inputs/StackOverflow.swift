func recurse(_ level: Int) {
  if level % 100000 == 0 {
    print(level)
  }
  recurse(level + 1)
}

@main
struct StackOverflow {
  static func main() {
    recurse(1)
  }
}
