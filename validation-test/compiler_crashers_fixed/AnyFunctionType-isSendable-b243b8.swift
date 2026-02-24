// {"kind":"typecheck","languageMode":6,"original":"1d5c6bbb","signature":"swift::AnyFunctionType::isSendable() const","signatureAssert":"Assertion failed: (!hasSendableDependentType() && \"Query Sendable dependence first\"), function isSendable"}
// RUN: not %target-swift-frontend -typecheck -swift-version 6 %s
.appending ?? \a
