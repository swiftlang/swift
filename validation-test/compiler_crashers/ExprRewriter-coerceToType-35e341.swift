// {"kind":"typecheck","languageMode":6,"noObjCInterop":true,"original":"0f871225","signature":"(anonymous namespace)::ExprRewriter::coerceToType(swift::Expr*, swift::Type, swift::constraints::ConstraintLocatorBuilder)","signatureAssert":"Assertion failed: (ctx.LangOpts.EnableObjCInterop && \"metatype-to-object conversion requires objc interop\"), function coerceToType","signatureNext":"Solution::coerceToType"}
// RUN: not --crash %target-swift-frontend -typecheck -disable-objc-interop -swift-version 6 %s
let a: AnyClass
func b<c>(c ) -> c
>
let d = b(a
