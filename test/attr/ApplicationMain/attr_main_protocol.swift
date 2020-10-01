// RUN: %target-swift-frontend -typecheck -parse-as-library -verify %s

@main // expected-error{{'@main' attribute cannot be applied to this declaration}}
protocol EntryPoint {
  static func main()
}
