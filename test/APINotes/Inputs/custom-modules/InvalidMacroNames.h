#define OPERATOR_NAMED_MACRO 23 // expected-warning {{custom Swift name '+' ignored because it is not valid for var; imported as 'OPERATOR_NAMED_MACRO' instead}}
#define FUNCTION_NAMED_MACRO 29 // expected-warning {{custom Swift name 'functionNamedMacro()' ignored because it is not valid for var; imported as 'FUNCTION_NAMED_MACRO' instead}}
#define MEMBER_NAMED_MACRO 31 // expected-warning {{custom Swift name 'Container.memberNamedMacro' ignored because it is not valid for var; imported as 'MEMBER_NAMED_MACRO' instead}}
