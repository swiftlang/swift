// RUN: %target-swift-frontend -c -parse-as-library %s -enable-implicit-dynamic

@main
struct BasicAppApp {
  static func main() {}
}
