// RUN: %swift %s -verify

typealias Recovery1

func flushDiagnostics1() {} // expected-error {{expected '=' in typealias declaration}}

typealias Recovery2 :

func flushDiagnostics2() {} // expected-error {{expected '=' in typealias declaration}} expected-error {{expected identifier for type name}}

typealias Recovery3 =

func flushDiagnostics3() {} // expected-error {{expected type in typealias declaration}}

typealias Recovery4 : Int

func flushDiagnostics4() {} // expected-error {{expected '=' in typealias declaration}}

typealias Recovery4 : Int, Float

func flushDiagnostics5() {} // expected-error {{expected '=' in typealias declaration}}

