// RUN: %empty-directory(%t)

// RUN: %target-build-swift \
// RUN:     -emit-module \
// RUN:     %S/Inputs/enum-multipayload-emptycase-storetag_Resilient.swift \
// RUN:     -parse-as-library \
// RUN:     -enable-library-evolution \
// RUN:     -module-name Resilient \
// RUN:     -emit-module-path %t/Resilient.swiftmodule

// RUN: %target-build-swift \
// RUN:     -c \
// RUN:     %S/Inputs/enum-multipayload-emptycase-storetag_Resilient.swift \
// RUN:     -parse-as-library \
// RUN:     -enable-library-evolution \
// RUN:     -module-name Resilient \
// RUN:     -o %t/Resilient.o

// RUN: %target-build-swift \
// RUN:     -c \
// RUN:     %s \
// RUN:     -o %t/main.o \
// RUN:     -I %t

// RUN: %target-swiftc_driver \
// RUN:     %t/Resilient.o \
// RUN:     %t/main.o \
// RUN:     -o %t/main

// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Resilient

func dump<T>(_ value: T) {
  print(value)
}

enum ProblematicEnumeration {
    case p0(Bewl)
    case p1(Bool)
    case e0
    case e1
    case e2
    case e3
    case e4
    case e5
    case e6
    case e7
    case e8
    case e9
    case e10
    case e11
    case e12
    case e13
    case e14
    case e15
    case e16
    case e17
    case e18
    case e19
    case e20
    case e21
    case e22
    case e23
    case e24
    case e25
    case e26
    case e27
    case e28
    case e29
    case e30
    case e31
    case e32
    case e33
    case e34
    case e35
    case e36
    case e37
    case e38
    case e39
    case e40
    case e41
    case e42
    case e43
    case e44
    case e45
    case e46
    case e47
    case e48
    case e49
    case e50
    case e51
    case e52
    case e53
    case e54
    case e55
    case e56
    case e57
    case e58
    case e59
    case e60
    case e61
    case e62
    case e63
    case e64
    case e65
    case e66
    case e67
    case e68
    case e69
    case e70
    case e71
    case e72
    case e73
    case e74
    case e75
    case e76
    case e77
    case e78
    case e79
    case e80
    case e81
    case e82
    case e83
    case e84
    case e85
    case e86
    case e87
    case e88
    case e89
    case e90
    case e91
    case e92
    case e93
    case e94
    case e95
    case e96
    case e97
    case e98
    case e99
    case e100
    case e101
    case e102
    case e103
    case e104
    case e105
    case e106
    case e107
    case e108
    case e109
    case e110
    case e111
    case e112
    case e113
    case e114
    case e115
    case e116
    case e117
    case e118
    case e119
    case e120
    case e121
    case e122
    case e123
    case e124
    case e125
    case e126
    case e127
    case e128
    case e129
    case e130
    case e131
    case e132
    case e133
    case e134
    case e135
    case e136
    case e137
    case e138
    case e139
    case e140
    case e141
    case e142
    case e143
    case e144
    case e145
    case e146
    case e147
    case e148
    case e149
    case e150
    case e151
    case e152
    case e153
    case e154
    case e155
    case e156
    case e157
    case e158
    case e159
    case e160
    case e161
    case e162
    case e163
    case e164
    case e165
    case e166
    case e167
    case e168
    case e169
    case e170
    case e171
    case e172
    case e173
    case e174
    case e175
    case e176
    case e177
    case e178
    case e179
    case e180
    case e181
    case e182
    case e183
    case e184
    case e185
    case e186
    case e187
    case e188
    case e189
    case e190
    case e191
    case e192
    case e193
    case e194
    case e195
    case e196
    case e197
    case e198
    case e199
    case e200
    case e201
    case e202
    case e203
    case e204
    case e205
    case e206
    case e207
    case e208
    case e209
    case e210
    case e211
    case e212
    case e213
    case e214
    case e215
    case e216
    case e217
    case e218
    case e219
    case e220
    case e221
    case e222
    case e223
    case e224
    case e225
    case e226
    case e227
    case e228
    case e229
    case e230
    case e231
    case e232
    case e233
    case e234
    case e235
    case e236
    case e237
    case e238
    case e239
    case e240
    case e241
    case e242
    case e243
    case e244
    case e245
    case e246
    case e247
    case e248
    case e249
    case e250
    case e251
    case e252
    case e253
    case e254
    case e255
    case e256
    case e257
}

func doit() {
    dump(ProblematicEnumeration.e0)
    dump(ProblematicEnumeration.e1)
    dump(ProblematicEnumeration.e2)
    dump(ProblematicEnumeration.e3)
    dump(ProblematicEnumeration.e4)
    dump(ProblematicEnumeration.e5)
    dump(ProblematicEnumeration.e6)
    dump(ProblematicEnumeration.e7)
    dump(ProblematicEnumeration.e8)
    dump(ProblematicEnumeration.e9)
    dump(ProblematicEnumeration.e10)
    dump(ProblematicEnumeration.e11)
    dump(ProblematicEnumeration.e12)
    dump(ProblematicEnumeration.e13)
    dump(ProblematicEnumeration.e14)
    dump(ProblematicEnumeration.e15)
    dump(ProblematicEnumeration.e16)
    dump(ProblematicEnumeration.e17)
    dump(ProblematicEnumeration.e18)
    dump(ProblematicEnumeration.e19)
    dump(ProblematicEnumeration.e20)
    dump(ProblematicEnumeration.e21)
    dump(ProblematicEnumeration.e22)
    dump(ProblematicEnumeration.e23)
    dump(ProblematicEnumeration.e24)
    dump(ProblematicEnumeration.e25)
    dump(ProblematicEnumeration.e26)
    dump(ProblematicEnumeration.e27)
    dump(ProblematicEnumeration.e28)
    dump(ProblematicEnumeration.e29)
    dump(ProblematicEnumeration.e30)
    dump(ProblematicEnumeration.e31)
    dump(ProblematicEnumeration.e32)
    dump(ProblematicEnumeration.e33)
    dump(ProblematicEnumeration.e34)
    dump(ProblematicEnumeration.e35)
    dump(ProblematicEnumeration.e36)
    dump(ProblematicEnumeration.e37)
    dump(ProblematicEnumeration.e38)
    dump(ProblematicEnumeration.e39)
    dump(ProblematicEnumeration.e40)
    dump(ProblematicEnumeration.e41)
    dump(ProblematicEnumeration.e42)
    dump(ProblematicEnumeration.e43)
    dump(ProblematicEnumeration.e44)
    dump(ProblematicEnumeration.e45)
    dump(ProblematicEnumeration.e46)
    dump(ProblematicEnumeration.e47)
    dump(ProblematicEnumeration.e48)
    dump(ProblematicEnumeration.e49)
    dump(ProblematicEnumeration.e50)
    dump(ProblematicEnumeration.e51)
    dump(ProblematicEnumeration.e52)
    dump(ProblematicEnumeration.e53)
    dump(ProblematicEnumeration.e54)
    dump(ProblematicEnumeration.e55)
    dump(ProblematicEnumeration.e56)
    dump(ProblematicEnumeration.e57)
    dump(ProblematicEnumeration.e58)
    dump(ProblematicEnumeration.e59)
    dump(ProblematicEnumeration.e60)
    dump(ProblematicEnumeration.e61)
    dump(ProblematicEnumeration.e62)
    dump(ProblematicEnumeration.e63)
    dump(ProblematicEnumeration.e64)
    dump(ProblematicEnumeration.e65)
    dump(ProblematicEnumeration.e66)
    dump(ProblematicEnumeration.e67)
    dump(ProblematicEnumeration.e68)
    dump(ProblematicEnumeration.e69)
    dump(ProblematicEnumeration.e70)
    dump(ProblematicEnumeration.e71)
    dump(ProblematicEnumeration.e72)
    dump(ProblematicEnumeration.e73)
    dump(ProblematicEnumeration.e74)
    dump(ProblematicEnumeration.e75)
    dump(ProblematicEnumeration.e76)
    dump(ProblematicEnumeration.e77)
    dump(ProblematicEnumeration.e78)
    dump(ProblematicEnumeration.e79)
    dump(ProblematicEnumeration.e80)
    dump(ProblematicEnumeration.e81)
    dump(ProblematicEnumeration.e82)
    dump(ProblematicEnumeration.e83)
    dump(ProblematicEnumeration.e84)
    dump(ProblematicEnumeration.e85)
    dump(ProblematicEnumeration.e86)
    dump(ProblematicEnumeration.e87)
    dump(ProblematicEnumeration.e88)
    dump(ProblematicEnumeration.e89)
    dump(ProblematicEnumeration.e90)
    dump(ProblematicEnumeration.e91)
    dump(ProblematicEnumeration.e92)
    dump(ProblematicEnumeration.e93)
    dump(ProblematicEnumeration.e94)
    dump(ProblematicEnumeration.e95)
    dump(ProblematicEnumeration.e96)
    dump(ProblematicEnumeration.e97)
    dump(ProblematicEnumeration.e98)
    dump(ProblematicEnumeration.e99)
    dump(ProblematicEnumeration.e100)
    dump(ProblematicEnumeration.e101)
    dump(ProblematicEnumeration.e102)
    dump(ProblematicEnumeration.e103)
    dump(ProblematicEnumeration.e104)
    dump(ProblematicEnumeration.e105)
    dump(ProblematicEnumeration.e106)
    dump(ProblematicEnumeration.e107)
    dump(ProblematicEnumeration.e108)
    dump(ProblematicEnumeration.e109)
    dump(ProblematicEnumeration.e110)
    dump(ProblematicEnumeration.e111)
    dump(ProblematicEnumeration.e112)
    dump(ProblematicEnumeration.e113)
    dump(ProblematicEnumeration.e114)
    dump(ProblematicEnumeration.e115)
    dump(ProblematicEnumeration.e116)
    dump(ProblematicEnumeration.e117)
    dump(ProblematicEnumeration.e118)
    dump(ProblematicEnumeration.e119)
    dump(ProblematicEnumeration.e120)
    dump(ProblematicEnumeration.e121)
    dump(ProblematicEnumeration.e122)
    dump(ProblematicEnumeration.e123)
    dump(ProblematicEnumeration.e124)
    dump(ProblematicEnumeration.e125)
    dump(ProblematicEnumeration.e126)
    dump(ProblematicEnumeration.e127)
    dump(ProblematicEnumeration.e128)
    dump(ProblematicEnumeration.e129)
    dump(ProblematicEnumeration.e130)
    dump(ProblematicEnumeration.e131)
    dump(ProblematicEnumeration.e132)
    dump(ProblematicEnumeration.e133)
    dump(ProblematicEnumeration.e134)
    dump(ProblematicEnumeration.e135)
    dump(ProblematicEnumeration.e136)
    dump(ProblematicEnumeration.e137)
    dump(ProblematicEnumeration.e138)
    dump(ProblematicEnumeration.e139)
    dump(ProblematicEnumeration.e140)
    dump(ProblematicEnumeration.e141)
    dump(ProblematicEnumeration.e142)
    dump(ProblematicEnumeration.e143)
    dump(ProblematicEnumeration.e144)
    dump(ProblematicEnumeration.e145)
    dump(ProblematicEnumeration.e146)
    dump(ProblematicEnumeration.e147)
    dump(ProblematicEnumeration.e148)
    dump(ProblematicEnumeration.e149)
    dump(ProblematicEnumeration.e150)
    dump(ProblematicEnumeration.e151)
    dump(ProblematicEnumeration.e152)
    dump(ProblematicEnumeration.e153)
    dump(ProblematicEnumeration.e154)
    dump(ProblematicEnumeration.e155)
    dump(ProblematicEnumeration.e156)
    dump(ProblematicEnumeration.e157)
    dump(ProblematicEnumeration.e158)
    dump(ProblematicEnumeration.e159)
    dump(ProblematicEnumeration.e160)
    dump(ProblematicEnumeration.e161)
    dump(ProblematicEnumeration.e162)
    dump(ProblematicEnumeration.e163)
    dump(ProblematicEnumeration.e164)
    dump(ProblematicEnumeration.e165)
    dump(ProblematicEnumeration.e166)
    dump(ProblematicEnumeration.e167)
    dump(ProblematicEnumeration.e168)
    dump(ProblematicEnumeration.e169)
    dump(ProblematicEnumeration.e170)
    dump(ProblematicEnumeration.e171)
    dump(ProblematicEnumeration.e172)
    dump(ProblematicEnumeration.e173)
    dump(ProblematicEnumeration.e174)
    dump(ProblematicEnumeration.e175)
    dump(ProblematicEnumeration.e176)
    dump(ProblematicEnumeration.e177)
    dump(ProblematicEnumeration.e178)
    dump(ProblematicEnumeration.e179)
    dump(ProblematicEnumeration.e180)
    dump(ProblematicEnumeration.e181)
    dump(ProblematicEnumeration.e182)
    dump(ProblematicEnumeration.e183)
    dump(ProblematicEnumeration.e184)
    dump(ProblematicEnumeration.e185)
    dump(ProblematicEnumeration.e186)
    dump(ProblematicEnumeration.e187)
    dump(ProblematicEnumeration.e188)
    dump(ProblematicEnumeration.e189)
    dump(ProblematicEnumeration.e190)
    dump(ProblematicEnumeration.e191)
    dump(ProblematicEnumeration.e192)
    dump(ProblematicEnumeration.e193)
    dump(ProblematicEnumeration.e194)
    dump(ProblematicEnumeration.e195)
    dump(ProblematicEnumeration.e196)
    dump(ProblematicEnumeration.e197)
    dump(ProblematicEnumeration.e198)
    dump(ProblematicEnumeration.e199)
    dump(ProblematicEnumeration.e200)
    dump(ProblematicEnumeration.e201)
    dump(ProblematicEnumeration.e202)
    dump(ProblematicEnumeration.e203)
    dump(ProblematicEnumeration.e204)
    dump(ProblematicEnumeration.e205)
    dump(ProblematicEnumeration.e206)
    dump(ProblematicEnumeration.e207)
    dump(ProblematicEnumeration.e208)
    dump(ProblematicEnumeration.e209)
    dump(ProblematicEnumeration.e210)
    dump(ProblematicEnumeration.e211)
    dump(ProblematicEnumeration.e212)
    dump(ProblematicEnumeration.e213)
    dump(ProblematicEnumeration.e214)
    dump(ProblematicEnumeration.e215)
    dump(ProblematicEnumeration.e216)
    dump(ProblematicEnumeration.e217)
    dump(ProblematicEnumeration.e218)
    dump(ProblematicEnumeration.e219)
    dump(ProblematicEnumeration.e220)
    dump(ProblematicEnumeration.e221)
    dump(ProblematicEnumeration.e222)
    dump(ProblematicEnumeration.e223)
    dump(ProblematicEnumeration.e224)
    dump(ProblematicEnumeration.e225)
    dump(ProblematicEnumeration.e226)
    dump(ProblematicEnumeration.e227)
    dump(ProblematicEnumeration.e228)
    dump(ProblematicEnumeration.e229)
    dump(ProblematicEnumeration.e230)
    dump(ProblematicEnumeration.e231)
    dump(ProblematicEnumeration.e232)
    dump(ProblematicEnumeration.e233)
    dump(ProblematicEnumeration.e234)
    dump(ProblematicEnumeration.e235)
    dump(ProblematicEnumeration.e236)
    dump(ProblematicEnumeration.e237)
    dump(ProblematicEnumeration.e238)
    dump(ProblematicEnumeration.e239)
    dump(ProblematicEnumeration.e240)
    dump(ProblematicEnumeration.e241)
    dump(ProblematicEnumeration.e242)
    dump(ProblematicEnumeration.e243)
    dump(ProblematicEnumeration.e244)
    dump(ProblematicEnumeration.e245)
    dump(ProblematicEnumeration.e246)
    dump(ProblematicEnumeration.e247)
    dump(ProblematicEnumeration.e248)
    dump(ProblematicEnumeration.e249)
    dump(ProblematicEnumeration.e250)
    dump(ProblematicEnumeration.e251)
    dump(ProblematicEnumeration.e252)
    dump(ProblematicEnumeration.e253)
    dump(ProblematicEnumeration.e254)
    dump(ProblematicEnumeration.e255)
    dump(ProblematicEnumeration.e256)
    dump(ProblematicEnumeration.e257)
}

doit()
// CHECK: e0
// CHECK: e1
// CHECK: e2
// CHECK: e3
// CHECK: e4
// CHECK: e5
// CHECK: e6
// CHECK: e7
// CHECK: e8
// CHECK: e9
// CHECK: e10
// CHECK: e11
// CHECK: e12
// CHECK: e13
// CHECK: e14
// CHECK: e15
// CHECK: e16
// CHECK: e17
// CHECK: e18
// CHECK: e19
// CHECK: e20
// CHECK: e21
// CHECK: e22
// CHECK: e23
// CHECK: e24
// CHECK: e25
// CHECK: e26
// CHECK: e27
// CHECK: e28
// CHECK: e29
// CHECK: e30
// CHECK: e31
// CHECK: e32
// CHECK: e33
// CHECK: e34
// CHECK: e35
// CHECK: e36
// CHECK: e37
// CHECK: e38
// CHECK: e39
// CHECK: e40
// CHECK: e41
// CHECK: e42
// CHECK: e43
// CHECK: e44
// CHECK: e45
// CHECK: e46
// CHECK: e47
// CHECK: e48
// CHECK: e49
// CHECK: e50
// CHECK: e51
// CHECK: e52
// CHECK: e53
// CHECK: e54
// CHECK: e55
// CHECK: e56
// CHECK: e57
// CHECK: e58
// CHECK: e59
// CHECK: e60
// CHECK: e61
// CHECK: e62
// CHECK: e63
// CHECK: e64
// CHECK: e65
// CHECK: e66
// CHECK: e67
// CHECK: e68
// CHECK: e69
// CHECK: e70
// CHECK: e71
// CHECK: e72
// CHECK: e73
// CHECK: e74
// CHECK: e75
// CHECK: e76
// CHECK: e77
// CHECK: e78
// CHECK: e79
// CHECK: e80
// CHECK: e81
// CHECK: e82
// CHECK: e83
// CHECK: e84
// CHECK: e85
// CHECK: e86
// CHECK: e87
// CHECK: e88
// CHECK: e89
// CHECK: e90
// CHECK: e91
// CHECK: e92
// CHECK: e93
// CHECK: e94
// CHECK: e95
// CHECK: e96
// CHECK: e97
// CHECK: e98
// CHECK: e99
// CHECK: e100
// CHECK: e101
// CHECK: e102
// CHECK: e103
// CHECK: e104
// CHECK: e105
// CHECK: e106
// CHECK: e107
// CHECK: e108
// CHECK: e109
// CHECK: e110
// CHECK: e111
// CHECK: e112
// CHECK: e113
// CHECK: e114
// CHECK: e115
// CHECK: e116
// CHECK: e117
// CHECK: e118
// CHECK: e119
// CHECK: e120
// CHECK: e121
// CHECK: e122
// CHECK: e123
// CHECK: e124
// CHECK: e125
// CHECK: e126
// CHECK: e127
// CHECK: e128
// CHECK: e129
// CHECK: e130
// CHECK: e131
// CHECK: e132
// CHECK: e133
// CHECK: e134
// CHECK: e135
// CHECK: e136
// CHECK: e137
// CHECK: e138
// CHECK: e139
// CHECK: e140
// CHECK: e141
// CHECK: e142
// CHECK: e143
// CHECK: e144
// CHECK: e145
// CHECK: e146
// CHECK: e147
// CHECK: e148
// CHECK: e149
// CHECK: e150
// CHECK: e151
// CHECK: e152
// CHECK: e153
// CHECK: e154
// CHECK: e155
// CHECK: e156
// CHECK: e157
// CHECK: e158
// CHECK: e159
// CHECK: e160
// CHECK: e161
// CHECK: e162
// CHECK: e163
// CHECK: e164
// CHECK: e165
// CHECK: e166
// CHECK: e167
// CHECK: e168
// CHECK: e169
// CHECK: e170
// CHECK: e171
// CHECK: e172
// CHECK: e173
// CHECK: e174
// CHECK: e175
// CHECK: e176
// CHECK: e177
// CHECK: e178
// CHECK: e179
// CHECK: e180
// CHECK: e181
// CHECK: e182
// CHECK: e183
// CHECK: e184
// CHECK: e185
// CHECK: e186
// CHECK: e187
// CHECK: e188
// CHECK: e189
// CHECK: e190
// CHECK: e191
// CHECK: e192
// CHECK: e193
// CHECK: e194
// CHECK: e195
// CHECK: e196
// CHECK: e197
// CHECK: e198
// CHECK: e199
// CHECK: e200
// CHECK: e201
// CHECK: e202
// CHECK: e203
// CHECK: e204
// CHECK: e205
// CHECK: e206
// CHECK: e207
// CHECK: e208
// CHECK: e209
// CHECK: e210
// CHECK: e211
// CHECK: e212
// CHECK: e213
// CHECK: e214
// CHECK: e215
// CHECK: e216
// CHECK: e217
// CHECK: e218
// CHECK: e219
// CHECK: e220
// CHECK: e221
// CHECK: e222
// CHECK: e223
// CHECK: e224
// CHECK: e225
// CHECK: e226
// CHECK: e227
// CHECK: e228
// CHECK: e229
// CHECK: e230
// CHECK: e231
// CHECK: e232
// CHECK: e233
// CHECK: e234
// CHECK: e235
// CHECK: e236
// CHECK: e237
// CHECK: e238
// CHECK: e239
// CHECK: e240
// CHECK: e241
// CHECK: e242
// CHECK: e243
// CHECK: e244
// CHECK: e245
// CHECK: e246
// CHECK: e247
// CHECK: e248
// CHECK: e249
// CHECK: e250
// CHECK: e251
// CHECK: e252
// CHECK: e253
// CHECK: e254
// CHECK: e255
// CHECK: e256
// CHECK: e257
