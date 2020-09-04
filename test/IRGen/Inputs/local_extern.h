static inline int _no_prior_var() {
  extern int var;
  return var;
}

static inline int _no_prior_func() {
  extern int func();
  return func();
}

static int prior_var = 1;
static inline int _prior_var() {
  extern int prior_var;
  return prior_var;
}

static inline int prior_func() { return 1; }
static inline int _prior_func() {
  extern int prior_func();
  return prior_func();
}
