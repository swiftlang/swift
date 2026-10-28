// RUN: %target-swift-frontend -c %s -o %t -swift-version 6 -enable-sil-opaque-values -Onone

func test() {
  let a: [([Int]) -> [Int]] = [
    { x in
      x.map {
        $0 + 1
      }
    }
  ]
  print(a)
}
