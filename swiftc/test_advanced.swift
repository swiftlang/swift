func greet(name: String) -> String {
    return "Hello, " + name
}

let userName = "Swift Developer"
let greeting = greet(name: userName)
print(greeting)

let numbers = [1, 2, 3, 4, 5]
let doubled = numbers.map { $0 * 2 }
print("Doubled numbers processed")
