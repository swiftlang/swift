#include <availability_domain.h>

int huron_pred(void);

CLANG_ENABLED_AVAILABILITY_DOMAIN(Salt);
CLANG_DISABLED_AVAILABILITY_DOMAIN(Erie);
CLANG_DYNAMIC_AVAILABILITY_DOMAIN(Huron, huron_pred);

#define AVAIL 0
#define UNAVAIL 1

__attribute__((availability(domain:Salt, AVAIL)))
void available_in_salt(void);

#undef UNAVAIL
#undef AVAIL
