// RUN: not %target-swift-frontend -typecheck %s
enum a {
  case (b: isolated c)
}
