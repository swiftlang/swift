// Note that we have deliberately misdeclared the return type of
// objc_autorelease to be 'void *' rather than 'id', to prevent ARC from trying
// to steal the returned object out of the autorelease pool.
void *objc_autorelease(id _Nonnull object __attribute__((ns_consumed)));
