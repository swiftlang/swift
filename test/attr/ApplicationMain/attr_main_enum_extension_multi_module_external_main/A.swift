// This file is a part of the multi-file test driven by 'main2.swift'.

// RUN: %target-swift-frontend -parse %s

public enum Main {
  public static func main() {
    print("ok")
  }
}
