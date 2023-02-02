@class Foo;
@class Baz;
@class ConflictingTypeName;
@class subscript;

void takeAFoo(Foo *foo);
Foo *returnAFoo();

void takeABaz(Baz *baz);
Baz *returnABaz();

void takeAConflictingTypeName(ConflictingTypeName *param);
ConflictingTypeName *returnAConflictingTypeName();

void takeASubscript(subscript *param);
subscript *returnASubscript();
