// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_Enum_MultiPayload_empty_payload_xi
// RUN: %target-codesign %t/reflect_Enum_MultiPayload_empty_payload_xi

// RUN: %target-run %target-swift-reflection-test %t/reflect_Enum_MultiPayload_empty_payload_xi | tee /dev/stderr | %FileCheck %s --check-prefix=CHECK --dump-input=fail

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

// Inner's `c(Void)` payload is zero-sized, so layout treats it as a non-payload
// case and it gets no tag value of its own: tags 0 and 1 hold the payload cases,
// tag 2 numbers the three non-payload cases, and tags 3 through 255 are extra
// inhabitants. Counting `c` as a payload case makes tag 3 look like valid
// content, which loses the last extra inhabitant.
enum Inner {
case a(Int8)
case b(Int8)
case c(Void)
case d
case e
}

// Outer has enough non-payload cases to reach Inner's last extra inhabitant.
enum Outer {
case payload(Inner)
case x0
case x1
case x2
case x3
case x4
case x5
case x6
case x7
case x8
case x9
case x10
case x11
case x12
case x13
case x14
case x15
case x16
case x17
case x18
case x19
case x20
case x21
case x22
case x23
case x24
case x25
case x26
case x27
case x28
case x29
case x30
case x31
case x32
case x33
case x34
case x35
case x36
case x37
case x38
case x39
case x40
case x41
case x42
case x43
case x44
case x45
case x46
case x47
case x48
case x49
case x50
case x51
case x52
case x53
case x54
case x55
case x56
case x57
case x58
case x59
case x60
case x61
case x62
case x63
case x64
case x65
case x66
case x67
case x68
case x69
case x70
case x71
case x72
case x73
case x74
case x75
case x76
case x77
case x78
case x79
case x80
case x81
case x82
case x83
case x84
case x85
case x86
case x87
case x88
case x89
case x90
case x91
case x92
case x93
case x94
case x95
case x96
case x97
case x98
case x99
case x100
case x101
case x102
case x103
case x104
case x105
case x106
case x107
case x108
case x109
case x110
case x111
case x112
case x113
case x114
case x115
case x116
case x117
case x118
case x119
case x120
case x121
case x122
case x123
case x124
case x125
case x126
case x127
case x128
case x129
case x130
case x131
case x132
case x133
case x134
case x135
case x136
case x137
case x138
case x139
case x140
case x141
case x142
case x143
case x144
case x145
case x146
case x147
case x148
case x149
case x150
case x151
case x152
case x153
case x154
case x155
case x156
case x157
case x158
case x159
case x160
case x161
case x162
case x163
case x164
case x165
case x166
case x167
case x168
case x169
case x170
case x171
case x172
case x173
case x174
case x175
case x176
case x177
case x178
case x179
case x180
case x181
case x182
case x183
case x184
case x185
case x186
case x187
case x188
case x189
case x190
case x191
case x192
case x193
case x194
case x195
case x196
case x197
case x198
case x199
case x200
case x201
case x202
case x203
case x204
case x205
case x206
case x207
case x208
case x209
case x210
case x211
case x212
case x213
case x214
case x215
case x216
case x217
case x218
case x219
case x220
case x221
case x222
case x223
case x224
case x225
case x226
case x227
case x228
case x229
case x230
case x231
case x232
case x233
case x234
case x235
case x236
case x237
case x238
case x239
case x240
case x241
case x242
case x243
case x244
case x245
case x246
case x247
case x248
case x249
case x250
case x251
case x252
}

reflect(enumValue: Outer.x252)

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_MultiPayload_empty_payload_xi.Outer)
// CHECK-NEXT: Value: .x252

reflect(enumValue: Outer.x0)

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_MultiPayload_empty_payload_xi.Outer)
// CHECK-NEXT: Value: .x0

reflect(enumValue: Outer.payload(.c(())))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_MultiPayload_empty_payload_xi.Outer)
// CHECK-NEXT: Value: .payload(.c(_))

reflect(enumValue: Outer.payload(.a(1)))

// CHECK: Reflecting an enum value.
// CHECK-NEXT: Type reference:
// CHECK-NEXT: (enum reflect_Enum_MultiPayload_empty_payload_xi.Outer)
// CHECK-NEXT: Value: .payload(.a(_))

doneReflecting()

// CHECK: Done.
