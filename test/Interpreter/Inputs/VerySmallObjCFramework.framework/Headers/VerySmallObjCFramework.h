extern int globalValue;

static const int staticGlobalValue = 1171;

const int *getLocation() {
  return &staticGlobalValue;
}
