// RUN: not %target-swift-frontend %s -c -parse-as-library

@main struct M1 {
  static func main() {}
}

@main struct M2<T> {
  static func main() {}
}

