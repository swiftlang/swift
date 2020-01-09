// RUN: %empty-directory(%t)
// RUN: %target-build-swift -lswiftSwiftReflectionTest %s -o %t/reflect_existential
// RUN: %target-codesign %t/reflect_existential

// RUN: %target-run %target-swift-reflection-test %t/reflect_existential | tee /dev/stderr | %FileCheck %s --check-prefix=CHECK-%target-ptrsize --dump-input=fail

// REQUIRES: objc_interop
// REQUIRES: executable_test

import SwiftReflectionTest

enum OneCaseNoPayload {
  case only
}
class OneCaseNoPayloadC {
  var x = OneCaseNoPayload.only
  var y = 42
}
reflect(object: OneCaseNoPayloadC())
// CHECK-64: abc
// CHECK-32: def


enum ManyCasesNoPayload {
  case a, b, c, d
}
class ManyCasesNoPayloadC {
  var a = ManyCasesNoPayload.a
  var b = ManyCasesNoPayload.b
  var c = ManyCasesNoPayload.c
  var d = ManyCasesNoPayload.d
  var s = "beeep"
}
reflect(object: ManyCasesNoPayloadC())

enum VastNumberOfCasesNoPayload {
  case option0
  case option1
  case option2
  case option3
  case option4
  case option5
  case option6
  case option7
  case option8
  case option9
  case option10
  case option11
  case option12
  case option13
  case option14
  case option15
  case option16
  case option17
  case option18
  case option19
  case option20
  case option21
  case option22
  case option23
  case option24
  case option25
  case option26
  case option27
  case option28
  case option29
  case option30
  case option31
  case option32
  case option33
  case option34
  case option35
  case option36
  case option37
  case option38
  case option39
  case option40
  case option41
  case option42
  case option43
  case option44
  case option45
  case option46
  case option47
  case option48
  case option49
  case option50
  case option51
  case option52
  case option53
  case option54
  case option55
  case option56
  case option57
  case option58
  case option59
  case option60
  case option61
  case option62
  case option63
  case option64
  case option65
  case option66
  case option67
  case option68
  case option69
  case option70
  case option71
  case option72
  case option73
  case option74
  case option75
  case option76
  case option77
  case option78
  case option79
  case option80
  case option81
  case option82
  case option83
  case option84
  case option85
  case option86
  case option87
  case option88
  case option89
  case option90
  case option91
  case option92
  case option93
  case option94
  case option95
  case option96
  case option97
  case option98
  case option99
  case option100
  case option101
  case option102
  case option103
  case option104
  case option105
  case option106
  case option107
  case option108
  case option109
  case option110
  case option111
  case option112
  case option113
  case option114
  case option115
  case option116
  case option117
  case option118
  case option119
  case option120
  case option121
  case option122
  case option123
  case option124
  case option125
  case option126
  case option127
  case option128
  case option129
  case option130
  case option131
  case option132
  case option133
  case option134
  case option135
  case option136
  case option137
  case option138
  case option139
  case option140
  case option141
  case option142
  case option143
  case option144
  case option145
  case option146
  case option147
  case option148
  case option149
  case option150
  case option151
  case option152
  case option153
  case option154
  case option155
  case option156
  case option157
  case option158
  case option159
  case option160
  case option161
  case option162
  case option163
  case option164
  case option165
  case option166
  case option167
  case option168
  case option169
  case option170
  case option171
  case option172
  case option173
  case option174
  case option175
  case option176
  case option177
  case option178
  case option179
  case option180
  case option181
  case option182
  case option183
  case option184
  case option185
  case option186
  case option187
  case option188
  case option189
  case option190
  case option191
  case option192
  case option193
  case option194
  case option195
  case option196
  case option197
  case option198
  case option199
  case option200
  case option201
  case option202
  case option203
  case option204
  case option205
  case option206
  case option207
  case option208
  case option209
  case option210
  case option211
  case option212
  case option213
  case option214
  case option215
  case option216
  case option217
  case option218
  case option219
  case option220
  case option221
  case option222
  case option223
  case option224
  case option225
  case option226
  case option227
  case option228
  case option229
  case option230
  case option231
  case option232
  case option233
  case option234
  case option235
  case option236
  case option237
  case option238
  case option239
  case option240
  case option241
  case option242
  case option243
  case option244
  case option245
  case option246
  case option247
  case option248
  case option249
  case option250
  case option251
  case option252
  case option253
  case option254
  case option255
  case option256
}


enum ManyCasesOnePayload {
  case payload(String)
  case otherA, otherB, otherC
}
class ManyCasesOnePayloadC {
  var payload = ManyCasesOnePayload.payload("testString")
  var a = ManyCasesOnePayload.otherA
  var b = ManyCasesOnePayload.otherB
  var c = ManyCasesOnePayload.otherC
}
reflect(object: ManyCasesOnePayloadC())


enum ManyCasesManyPayloads {
  case a(String)
  case b([Int])
  case c([String: String])
}
class ManyCasesManyPayloadsC {
  var a = ManyCasesManyPayloads.a("testString")
  var b = ManyCasesManyPayloads.b([10, 20, 30])
  var c = ManyCasesManyPayloads.c(["name": "Telephone", "purpose": "Bothering"])
}
reflect(object: ManyCasesManyPayloadsC())

reflect(any: OneCaseNoPayload.only)
reflect(any: ManyCasesNoPayload.b)
reflect(any: VastNumberOfCasesNoPayload.option12)
reflect(any: VastNumberOfCasesNoPayload.option256)
reflect(any: ManyCasesOnePayload.payload("hello, world"))
reflect(any: ManyCasesManyPayloads.a("hi, world"))


doneReflecting()
