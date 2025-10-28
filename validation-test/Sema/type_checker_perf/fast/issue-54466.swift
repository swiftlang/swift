// RUN: %target-typecheck-verify-swift -solver-scope-threshold=25000
// Succeeds in 125ms with 19934 scopes

// https://github.com/swiftlang/swift/issues/54466

func slow() {
  let items = (1...10).map { _ in
   (weight: Int.random(in: 1...10), value: Int.random(in: 1...10))
  }
  let _ = Dictionary(grouping: items, by: {$0.weight})
   .mapValues{$0.map{$0.1}}
   .map{ item in
   (weight: item.key, value: item.value.filter{
   $0 >= item.key
   })
   }
   .filter{!$0.value.isEmpty}
   .sorted{$0.weight < $1.weight}
   .map{($0.weight, $0.value.sorted{$0 > $1})}
}
