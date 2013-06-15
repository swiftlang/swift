// RUN: %swift -i %s | FileCheck %s

// Dictionary literal
var dict = ["Hello" : 1, "World" : 2]

// Insertion
dict["Swift"] = 3

// Access
// CHECK: "Hello" => 1
print("\"Hello\" => " + String(dict["Hello"]) + "\n")
// CHECK: "Swift" => 3
print("\"Swift\" => " + String(dict["Swift"]) + "\n")
// CHECK: "World" => 2
print("\"World\" => " + String(dict["World"]) + "\n")

// Overwriting existing value
dict["Hello"] = 0
// CHECK: "Hello" => 0
print("\"Hello\" => " + String(dict["Hello"]) + "\n")
// CHECK: "Swift" => 3
print("\"Swift\" => " + String(dict["Swift"]) + "\n")
// CHECK: "World" => 2
print("\"World\" => " + String(dict["World"]) + "\n")
