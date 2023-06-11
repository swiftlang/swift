// RUN: %target-run-simple-swift(-parse-as-library -enable-experimental-feature SymbolLinkageMarkers) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_in_compiler

@_used
@_section("__TEXT,__mysection")
let my_global1: Int = 42

@_used
@_section("__TEXT,__mysection")
let my_global2: Int = 46

@_silgen_name(raw: "section$start$__TEXT$__mysection")
var mysection_start: Int

@_silgen_name(raw: "section$end$__TEXT$__mysection")
var mysection_end: Int

@main
struct Main {
	static func main() {
		let start = UnsafeRawPointer(&mysection_start)
		let end = UnsafeRawPointer(&mysection_end)
		let size = end - start
		let count = size / (Int.bitWidth / 8)
		print("count: \(count)")
		let linker_set = UnsafeBufferPointer(start: start.bindMemory(to: Int.self, capacity: count), count: count)
		for i in 0 ..< linker_set.count {
			print("mysection[\(i)]: \(linker_set[i])")
		}
	}
}

// CHECK: count: 2
// CHECK: mysection[0]: 42
// CHECK: mysection[1]: 46
