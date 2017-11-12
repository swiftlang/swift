typedef void (^PlainBlock)(void);
typedef void (^BlockWithEscapingParam)(PlainBlock);
typedef void (^BlockWithNoescapeParam)(__attribute__((noescape)) PlainBlock);
typedef BlockWithEscapingParam (^BlockReturningBlockWithEscapingParam)(void);
typedef BlockWithNoescapeParam (^BlockReturningBlockWithNoescapeParam)(void);
