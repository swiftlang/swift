// RUN: rm -rf %t && mkdir -p %t && %gyb -DWORD_BITS=%target-ptrsize %s -o %t/out.swift
// RUN: %line-directive %t/out.swift -- %target-build-swift %t/out.swift -o %t/a.out -Onone -g
// RUN: %line-directive %t/out.swift -- %target-run %t/a.out
// REQUIRES: executable_test

import StdlibUnittest

var suite = TestSuite("AnyUnicode")

func printCU(_ x: UInt16) {
		print(String(x, radix: 16))	
}

suite.test("Angstrom") {
	let strNonNormal = "Å" // 0x212b
	let strNFD = "Å" // 0x0041 0x030a
	let strNFC = "Å" // 0x00c5

	print("NonNormal")
	for cu in strNonNormal.content.utf16 {
		printCU(cu)
	}

	print("NFD")
	for cu in strNFD.content.utf16 {
		printCU(cu)
	}

	print("NFD")
	for cu in strNFC.content.fccNormalizedUTF16 {
		printCU(cu)
	}

	print("non-normal => FCC")
	for cu in strNonNormal.content.fccNormalizedUTF16 {
		printCU(cu)
	}
	print("NFD => FCC")
	for cu in strNFD.content.fccNormalizedUTF16 {
		printCU(cu)
	}
	print("NFC => FCC")
	for cu in strNFC.content.fccNormalizedUTF16 {
		printCU(cu)
	}

	print(strNonNormal == strNFC)
	print(strNonNormal == strNFD)
	print(strNFC == strNFD)

	expectTrue(false)

}

runAllTests()