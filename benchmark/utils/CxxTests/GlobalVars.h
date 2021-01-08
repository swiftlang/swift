#ifndef BENCHMARK_GLOBAL_VARS_H
#define BENCHMARK_GLOBAL_VARS_H

static int globalInt = 42;
static constexpr int globalConstexprInt = 42;
static float globalFloat = 42;
static constexpr float globalConstexprFloat = 42;

struct BigObject {
  char buff[512];
};
static BigObject globalBigObject = BigObject();
static constexpr BigObject globalConstexprBigObject = BigObject();

#endif // BENCHMARK_GLOBAL_VARS_H
