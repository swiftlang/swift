var /*var-y:def*/y = 3
/*var-y*/y = /*var-y*/y + 2 + /*var-y*/y

struct Struct {
	let /*ivar-x:def*/x: Int
	var y: Int

	func sum() -> Int {
		return /*ivar-x*/x + y
	}
}

let aStruct = Struct(x: 10, y: 11)
print(aStruct . /*ivar-x*/x + aStruct.y)
var cat = {(a: Int) in aStruct . /*ivar-x*/x}

var aTuple = (1, 1)

switch aTuple {
case (let /*pattern-a:def*/a, let b):
	print(/*pattern-a*/a + b)
}

var opt = Optional.some(1)
var opt2 = Optional.some(2)

if let i = opt, let /*var-j:def*/j = opt2 {
	print(i + /*var-j*/j)
}

var (a, /*pattern-b:def*/b) = (1, 2)
print(a + /*pattern-b*/b)

struct S {
	lazy var lazyVal: Int = {
		let /*lazy:def*/myVal = 0
		return /*lazy:ref*/myVal
	}()
}

// REQUIRES: swift_swift_parser
// RUN: %empty-directory(%t.result)
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="var-y" -old-name "y" -new-name "yack" >> %t.result/variables_var-y.swift
// RUN: diff -u %S/Outputs/variables/var-y.swift.expected %t.result/variables_var-y.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="ivar-x" -old-name "x" -new-name "fox" >> %t.result/variables_ivar-x.swift
// RUN: diff -u %S/Outputs/variables/ivar-x.swift.expected %t.result/variables_ivar-x.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="pattern-a" -old-name "a" -new-name "axolotl" >> %t.result/variables_pattern-a.swift
// RUN: diff -u %S/Outputs/variables/pattern-a.swift.expected %t.result/variables_pattern-a.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="var-j" -old-name "j" -new-name "jackalope" >> %t.result/variables_var-j.swift
// RUN: diff -u %S/Outputs/variables/var-j.swift.expected %t.result/variables_var-j.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="pattern-b" -old-name "b" -new-name "bee" >> %t.result/variables_pattern-b.swift
// RUN: diff -u %S/Outputs/variables/pattern-b.swift.expected %t.result/variables_pattern-b.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="lazy" -old-name "myVal" -new-name "myNewVal" >> %t.result/variables_lazy.swift
// RUN: diff -u %S/Outputs/variables/lazy.swift.expected %t.result/variables_lazy.swift
