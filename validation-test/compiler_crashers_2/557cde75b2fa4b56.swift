// {"signature":"isParamListRepresentableInLanguage(swift::AbstractFunctionDecl const*, swift::ParameterList const*, swift::ObjCReason)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
class a { @objc b(@c & d Int) @propertyWrapper enum c
