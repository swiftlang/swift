#include <availability_domain.h>

int aegean_pred(void);

CLANG_ENABLED_AVAILABILITY_DOMAIN(Baltic);
CLANG_DISABLED_AVAILABILITY_DOMAIN(Mediterranean);
CLANG_DYNAMIC_AVAILABILITY_DOMAIN(Aegean, aegean_pred);

#define AVAIL 0
#define UNAVAIL 1

__attribute__((availability(domain:Baltic, AVAIL)))
void available_in_baltic(void);

#undef UNAVAIL
#undef AVAIL
