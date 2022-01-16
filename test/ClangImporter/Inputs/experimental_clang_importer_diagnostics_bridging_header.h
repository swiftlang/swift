// A briding header defining a small sample of unimportable declarations.
@class ForwardDeclaredInterface;

ForwardDeclaredInterface* CFunctionReturningAForwardDeclaredInterface();

#define FUNC_LIKE_MACRO() 0

struct PartialImport {
  int a;
  int b;
  int _Complex c;
  int _Complex d;
};

_Complex int unsupported_return_type();
