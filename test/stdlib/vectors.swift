// RUN: %swift %s -verify

//===----------------------------------------------------------------------===//
// Vector tests and samples.
//===----------------------------------------------------------------------===//

// Vec2i8: 8 bit signed integer vectors
var A = Vec2i8(2, 4)
var A1 = Vec2i8(2)
var A2 : Vec2i8
A2[0] = 5
A2[1] = A2[0]
var R = A * A1
var R1 = A / A1
var R2 = A % A1
var R3 = A - A1
var R4 = ~A1
var R5 = !R2
var R6 = A & R2
var R7 = A | A1
var R8 = A ^ A1
var R9 = +A
var R10 = -A2
var R11 = A + A2
A += R1
A -= R2
A1 /= R3
A2 *= R4
R5 %= A1
var X = A < A1
var X1 = A <= A1
var X2 = A > A2
var X3 = A >= A2
var X4 = A == A1
var R12 = A << A1
var R13 = A >> A2
var R14 = A & A2
var R15 = A | A2
var R16 = A ^ A2


// Vec2b: vector of two bools
var B = Vec2b(false, true)
var B1 = Vec2b(true)
var B2 = Vec2b(B[0], B1[1])
B2[0] = B1[1]
var B3 = B & B1
var B4 = B | B2
var B5 = B3 ^ B2

// Vec2f: vector of two 32-bit floats
var C = Vec2f(1, 2)
var C1 = Vec2f(3)
var C2 = Vec2f(C[0])
C[1] = 3
var C3 = +C
var C4 = -C
var C5 = C1 / C2
var C6 = C2 * C3
var C7 = C3 + C4
var C8 = C5 - C6
C8 += C1
C7 -= C2
C4 *= C5
C6 /= C7
var C9 = C < C2
var C10 = C <= C2
var C11 = C1 > C2
var C12 = C2 >= C3
var C13 = C1 == C3
var C14 = C2 != C3
