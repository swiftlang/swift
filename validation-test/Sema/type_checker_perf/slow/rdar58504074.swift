// RUN: %target-typecheck-verify-swift -solver-scope-threshold=5000

// Valid but slow

class A {}

extension A {
    class Item {
        let title: String
        let subtitle: String
        let value: String
        
        init(title: String, subtitle: String, value: String) {
            self.title = title
            self.subtitle = subtitle
            self.value = value
        }
    }
}

// expected-error@+1 {{reasonable time}}
let items: [A.Item] = (0...100).map { (number) -> A.Item in
    return A.Item(
        title: String(number),
        subtitle: String(number + number),
        value: String(number * number))
}

