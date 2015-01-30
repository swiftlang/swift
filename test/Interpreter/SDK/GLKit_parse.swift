// RUN: %target-build-swift -parse %s -Xfrontend -verify

// REQUIRES: objc_interop

import GLKit

var v2: GLKVector2

_ = v2.x
_ = v2.y
_ = v2.z // expected-error {{does not have a member}}
_ = v2.w // expected-error {{does not have a member}}

_ = v2.s
_ = v2.t
_ = v2.p // expected-error {{does not have a member}}
_ = v2.q // expected-error {{does not have a member}}

// Vector2 does not have 'r' or 'g' components
_ = v2.r // expected-error {{does not have a member}}
_ = v2.g // expected-error {{does not have a member}}
_ = v2.b // expected-error {{does not have a member}}
_ = v2.a // expected-error {{does not have a member}}

_ = v2[0]
_ = v2[1]

var v3: GLKVector3

_ = v3.x
_ = v3.y
_ = v3.z
_ = v3.w // expected-error {{does not have a member}}

_ = v3.s
_ = v3.t
_ = v3.p
_ = v3.q // expected-error {{does not have a member}}

_ = v3.r
_ = v3.g
_ = v3.b
_ = v3.a // expected-error {{does not have a member}}

_ = v3[0]
_ = v3[1]
_ = v3[2]

var v4: GLKVector4

_ = v4.x
_ = v4.y
_ = v4.z
_ = v4.w

_ = v4.s
_ = v4.t
_ = v4.p
_ = v4.q

_ = v4.r
_ = v4.g
_ = v4.b
_ = v4.a

_ = v4[0]
_ = v4[1]
_ = v4[2]
_ = v4[3]

var q: GLKQuaternion

_ = q.x
_ = q.y
_ = q.z
_ = q.w

_ = q.v.x
_ = q.v.y
_ = q.v.z
_ = q.v.w // expected-error {{does not have a member}}
_ = q.s

_ = q[0]
_ = q[1]
_ = q[2]
_ = q[3]

var m2: GLKMatrix2
_ = m2.m00
_ = m2.m01
_ = m2.m02 // expected-error {{does not have a member}}
_ = m2.m10
_ = m2.m11
_ = m2.m12 // expected-error {{does not have a member}}
_ = m2.m20 // expected-error {{does not have a member}}
_ = m2[0]
_ = m2[1]
_ = m2[2]
_ = m2[3]

var m3: GLKMatrix3
_ = m3.m00
_ = m3.m01
_ = m3.m02
_ = m3.m03 // expected-error {{does not have a member}}
_ = m3.m10
_ = m3.m11
_ = m3.m12
_ = m3.m13 // expected-error {{does not have a member}}
_ = m3.m20
_ = m3.m21
_ = m3.m22
_ = m3.m23 // expected-error {{does not have a member}}
_ = m3.m30 // expected-error {{does not have a member}}

_ = m3[0]
_ = m3[1]
_ = m3[2]
_ = m3[3]
_ = m3[4]
_ = m3[5]
_ = m3[6]
_ = m3[7]
_ = m3[8]

var m4: GLKMatrix4
_ = m4.m00
_ = m4.m01
_ = m4.m02
_ = m4.m03
_ = m4.m04 // expected-error {{does not have a member}}
_ = m4.m10
_ = m4.m11
_ = m4.m12
_ = m4.m13
_ = m4.m14 // expected-error {{does not have a member}}
_ = m4.m20
_ = m4.m21
_ = m4.m22
_ = m4.m23
_ = m4.m24 // expected-error {{does not have a member}}
_ = m4.m30
_ = m4.m31
_ = m4.m32
_ = m4.m33
_ = m4.m34 // expected-error {{does not have a member}}
_ = m4.m40 // expected-error {{does not have a member}}

_ = m4[ 0]
_ = m4[ 1]
_ = m4[ 2]
_ = m4[ 3]
_ = m4[ 4]
_ = m4[ 5]
_ = m4[ 6]
_ = m4[ 7]
_ = m4[ 8]
_ = m4[ 9]
_ = m4[10]
_ = m4[11]
_ = m4[12]
_ = m4[13]
_ = m4[14]
_ = m4[15]

