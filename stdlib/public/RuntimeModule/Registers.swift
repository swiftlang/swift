//===--- Registers.swift - Dwarf register mapping -------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Holds enums that define DWARF register mappings for the architectures we
// care about.
//
//===----------------------------------------------------------------------===//

import Swift

// .. x86-64 .................................................................

// https://gitlab.com/x86-psABIs/x86-64-ABI
@_spi(Registers) public enum X86_64Register: Int, Strideable, Comparable {

  public func advanced(by n: Int) -> X86_64Register {
    return X86_64Register(rawValue: self.rawValue + n)!
  }

  public func distance(to other: X86_64Register) -> Int {
    return other.rawValue - self.rawValue
  }

  public static func < (lhs: Self, rhs: Self) -> Bool {
    return lhs.rawValue < rhs.rawValue
  }

case rax = 0
case rdx = 1
case rcx = 2
case rbx = 3
case rsi = 4
case rdi = 5
case rbp = 6
case rsp = 7
case r8  = 8
case r9  = 9
case r10 = 10
case r11 = 11
case r12 = 12
case r13 = 13
case r14 = 14
case r15 = 15
case ra = 16
case xmm0 = 17
case xmm1 = 18
case xmm2 = 19
case xmm3 = 20
case xmm4 = 21
case xmm5 = 22
case xmm6 = 23
case xmm7 = 24
case xmm8 = 25
case xmm9 = 26
case xmm10 = 27
case xmm11 = 28
case xmm12 = 29
case xmm13 = 30
case xmm14 = 31
case xmm15 = 32
case st0 = 33
case st1 = 34
case st2 = 35
case st3 = 36
case st4 = 37
case st5 = 38
case st6 = 39
case st7 = 40
case mm0 = 41
case mm1 = 42
case mm2 = 43
case mm3 = 44
case mm4 = 45
case mm5 = 46
case mm6 = 47
case mm7 = 48
case rflags = 49
case es = 50
case cs = 51
case ss = 52
case ds = 53
case fs = 54
case gs = 55
  // 56-57 are reserved
case fs_base = 58
case gs_base = 59
  // 60-61 are reserved
case tr = 62
case ldtr = 63
case mxcsr = 64
case fcw = 65
case fsw = 66
case xmm16 = 67
case xmm17 = 68
case xmm18 = 69
case xmm19 = 70
case xmm20 = 71
case xmm21 = 72
case xmm22 = 73
case xmm23 = 74
case xmm24 = 75
case xmm25 = 76
case xmm26 = 77
case xmm27 = 78
case xmm28 = 79
case xmm29 = 80
case xmm30 = 81
case xmm31 = 82
  // 83-117 are reserved
case k0 = 118
case k1 = 119
case k2 = 120
case k3 = 121
case k4 = 122
case k5 = 123
case k6 = 124
case k7 = 125
  // 126-129 are reserved
}

// .. i386 ...................................................................

// https://gitlab.com/x86-psABIs/i386-ABI
@_spi(Registers) public enum I386Register: Int, Strideable, Comparable {

  public func advanced(by n: Int) -> I386Register {
    return I386Register(rawValue: self.rawValue + n)!
  }

  public func distance(to other: I386Register) -> Int {
    return other.rawValue - self.rawValue
  }

  public static func < (lhs: Self, rhs: Self) -> Bool {
    return lhs.rawValue < rhs.rawValue
  }

case eax = 0
case ecx = 1
case edx = 2
case ebx = 3
case esp = 4
case ebp = 5
case esi = 6
case edi = 7
case ra = 8
case eflags = 9
  // 10 is reserved
case st0 = 11
case st1 = 12
case st2 = 13
case st3 = 14
case st4 = 15
case st5 = 16
case st6 = 17
case st7 = 18
  // 19-20 are reserved
case xmm0 = 21
case xmm1 = 22
case xmm2 = 23
case xmm3 = 24
case xmm4 = 25
case xmm5 = 26
case xmm6 = 27
case xmm7 = 28
case mm0 = 29
case mm1 = 30
case mm2 = 31
case mm3 = 32
case mm4 = 33
case mm5 = 34
case mm6 = 35
case mm7 = 36
  // 36-38 are reserved
case mxcsr = 39
case es = 40
case cs = 41
case ss = 42
case ds = 43
case fs = 44
case gs = 45
  // 46-47 are reserved
case tr = 48
case ldtr = 49
  // 50-92 are reserved
case fs_base = 93
case gs_base = 94
}

// .. arm64 ..................................................................

// https://github.com/ARM-software/abi-aa/tree/main/aadwarf64
@_spi(Registers) public enum ARM64Register: Int, Strideable, Comparable {

  public func advanced(by n: Int) -> ARM64Register {
    return ARM64Register(rawValue: self.rawValue + n)!
  }

  public func distance(to other: ARM64Register) -> Int {
    return other.rawValue - self.rawValue
  }

  public static func < (lhs: Self, rhs: Self) -> Bool {
    return lhs.rawValue < rhs.rawValue
  }

case x0 = 0
case x1 = 1
case x2 = 2
case x3 = 3
case x4 = 4
case x5 = 5
case x6 = 6
case x7 = 7
case x8 = 8
case x9 = 9
case x10 = 10
case x11 = 11
case x12 = 12
case x13 = 13
case x14 = 14
case x15 = 15
case x16 = 16
case x17 = 17
case x18 = 18
case x19 = 19
case x20 = 20
case x21 = 21
case x22 = 22
case x23 = 23
case x24 = 24
case x25 = 25
case x26 = 26
case x27 = 27
case x28 = 28
case x29 = 29 // fp
case x30 = 30 // lr
case sp = 31  // x31
case pc = 32
case elr_mode = 33
case ra_sign_state = 34
case tpidrro_el0 = 35
case tpidr_el0 = 36
case tpidr_el1 = 37
case tpidr_el2 = 38
case tpidr_el3 = 39
  // 40-45 are reserved
case vg = 46
case ffr = 47
case p0 = 48
case p1 = 49
case p2 = 50
case p3 = 51
case p4 = 52
case p5 = 53
case p6 = 54
case p7 = 55
case p8 = 56
case p9 = 57
case p10 = 58
case p11 = 59
case p12 = 60
case p13 = 61
case p14 = 62
case p15 = 63
case v0 = 64
case v1 = 65
case v2 = 66
case v3 = 67
case v4 = 68
case v5 = 69
case v6 = 70
case v7 = 71
case v8 = 72
case v9 = 73
case v10 = 74
case v11 = 75
case v12 = 76
case v13 = 77
case v14 = 78
case v15 = 79
case v16 = 80
case v17 = 81
case v18 = 82
case v19 = 83
case v20 = 84
case v21 = 85
case v22 = 86
case v23 = 87
case v24 = 88
case v25 = 89
case v26 = 90
case v27 = 91
case v28 = 92
case v29 = 93
case v30 = 94
case v31 = 95
case z0 = 96
case z1 = 97
case z2 = 98
case z3 = 99
case z4 = 100
case z5 = 101
case z6 = 102
case z7 = 103
case z8 = 104
case z9 = 105
case z10 = 106
case z11 = 107
case z12 = 108
case z13 = 109
case z14 = 110
case z15 = 111
case z16 = 112
case z17 = 113
case z18 = 114
case z19 = 115
case z20 = 116
case z21 = 117
case z22 = 118
case z23 = 119
case z24 = 120
case z25 = 121
case z26 = 122
case z27 = 123
case z28 = 124
case z29 = 125
case z30 = 126
case z31 = 127
}

// .. arm ....................................................................

// https://github.com/ARM-software/abi-aa/tree/main/aadwarf32
@_spi(Registers) public enum ARMRegister: Int, Strideable, Comparable {

  public func advanced(by n: Int) -> ARMRegister {
    return ARMRegister(rawValue: self.rawValue + n)!
  }

  public func distance(to other: ARMRegister) -> Int {
    return other.rawValue - self.rawValue
  }

  public static func < (lhs: Self, rhs: Self) -> Bool {
    return lhs.rawValue < rhs.rawValue
  }

case r0 = 0
case r1 = 1
case r2 = 2
case r3 = 3
case r4 = 4
case r5 = 5
case r6 = 6
case r7 = 7
case r8 = 8
case r9 = 9
case r10 = 10
case r11 = 11 // fp
case r12 = 12 // ip - scratch register (NOT "instruction pointer")
case r13 = 13 // sp
case r14 = 14 // lr
case r15 = 15 // pc

  // Obsolescent, overlapping mappings for FPA and VFP
case old_f0_s0 = 16
case old_f1_s1 = 17
case old_f2_s2 = 18
case old_f3_s3 = 19
case old_f4_s4 = 20
case old_f5_s5 = 21
case old_f6_s6 = 22
case old_f7_s7 = 23
case old_s8 = 24
case old_s9 = 25
case old_s10 = 26
case old_s11 = 27
case old_s12 = 28
case old_s13 = 29
case old_s14 = 30
case old_s15 = 31
case old_s16 = 32
case old_s17 = 33
case old_s18 = 34
case old_s19 = 35
case old_s20 = 36
case old_s21 = 37
case old_s22 = 38
case old_s23 = 39
case old_s24 = 40
case old_s25 = 41
case old_s26 = 42
case old_s27 = 43
case old_s28 = 44
case old_s29 = 45
case old_s30 = 46
case old_s31 = 47

  // Legacy VFPv2
case s0 = 64
case s1 = 65
case s2 = 66
case s3 = 67
case s4 = 68
case s5 = 69
case s6 = 70
case s7 = 71
case s8 = 72
case s9 = 73
case s10 = 74
case s11 = 75
case s12 = 76
case s13 = 77
case s14 = 78
case s15 = 79
case s16 = 80
case s17 = 81
case s18 = 82
case s19 = 83
case s20 = 84
case s21 = 85
case s22 = 86
case s23 = 87
case s24 = 88
case s25 = 89
case s26 = 90
case s27 = 91
case s28 = 92
case s29 = 93
case s30 = 94
case s31 = 95

  // Obsolescent FPA registers
case f0 = 96
case f1 = 97
case f2 = 98
case f3 = 99
case f4 = 100
case f5 = 101
case f6 = 102
case f7 = 103

  // Intel wireless MMX GPRs / XScale accumulators
case wcgr0_acc0 = 104
case wcgr1_acc1 = 105
case wcgr2_acc2 = 106
case wcgr3_acc3 = 107
case wcgr4_acc4 = 108
case wcgr5_acc5 = 109
case wcgr6_acc6 = 110
case wcgr7_acc7 = 111

  // Intel wireless MMX data registers
case wr0 = 112
case wr1 = 113
case wr2 = 114
case wr3 = 115
case wr4 = 116
case wr5 = 117
case wr6 = 118
case wr7 = 119
case wr8 = 120
case wr9 = 121
case wr10 = 122
case wr11 = 123
case wr12 = 124
case wr13 = 125
case wr14 = 126
case wr15 = 127

case spsr = 128
case spsr_fiq = 129
case spsr_irq = 130
case spsr_abt = 131
case spsr_und = 132
case spsr_svc = 133

  // 134-142 are reserved

case ra_auth_code = 143

case r8_usr = 144
case r9_usr = 145
case r10_usr = 146
case r11_usr = 147
case r12_usr = 148
case r13_usr = 149
case r14_usr = 150

case r8_fiq = 151
case r9_fiq = 152
case r10_fiq = 153
case r11_fiq = 154
case r12_fiq = 155
case r13_fiq = 156
case r14_fiq = 157

case r13_irq = 158
case r14_irq = 159

case r13_abt = 160
case r14_abt = 161

case r13_und = 162
case r14_und = 163

case r13_svc = 164
case r14_svc = 165

  // 166-191 are reserved

  // Intel wqireless MMX control register
case wc0 = 192
case wc1 = 193
case wc2 = 194
case wc3 = 195
case wc4 = 196
case wc5 = 197
case wc6 = 198
case wc7 = 199

  // 200-255 are reserved

case d0 = 256
case d1 = 257
case d2 = 258
case d3 = 259
case d4 = 260
case d5 = 261
case d6 = 262
case d7 = 263
case d8 = 264
case d9 = 265
case d10 = 266
case d11 = 267
case d12 = 268
case d13 = 269
case d14 = 270
case d15 = 271
case d16 = 272
case d17 = 273
case d18 = 274
case d19 = 275
case d20 = 276
case d21 = 277
case d22 = 278
case d23 = 279
case d24 = 280
case d25 = 281
case d26 = 282
case d27 = 283
case d28 = 284
case d29 = 285
case d30 = 286
case d31 = 287

  // 288-319 are reserved

case tpidruro = 320
case tpidrurw = 321
case tpidpr = 322
case htpidpr = 323

  // 324-8191 are reserved
  // 8192-16383 are for vendor co-processors
}

#if arch(x86_64)
@_spi(Registers) public typealias HostRegister = X86_64Register
#elseif arch(i386)
@_spi(Registers) public typealias HostRegister = I386Register
#elseif arch(arm64) || arch(arm64_32)
@_spi(Registers) public typealias HostRegister = ARM64Register
#elseif arch(arm)
@_spi(Registers) public typealias HostRegister = ARMRegister
#endif
