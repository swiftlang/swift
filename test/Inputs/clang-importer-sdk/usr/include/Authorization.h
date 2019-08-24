typedef const struct AuthorizationOpaqueRef *AuthorizationRef;
int AuthorizationCreate(AuthorizationRef *authorization);
int AuthorizationFree(AuthorizationRef authorization);
