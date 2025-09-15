#include <availability_domain.h>

int aegean_pred(void);

CLANG_ENABLED_AVAILABILITY_DOMAIN(Baltic);
CLANG_ALWAYS_ENABLED_AVAILABILITY_DOMAIN(Bering);
CLANG_DISABLED_AVAILABILITY_DOMAIN(Mediterranean);
CLANG_DYNAMIC_AVAILABILITY_DOMAIN(Aegean, aegean_pred);

#define AVAIL 0
#define UNAVAIL 1

__attribute__((availability(domain:Baltic, AVAIL)))
void available_in_baltic(void);

__attribute__((availability(domain:Bering, AVAIL)))
void available_in_bering(void);

__attribute__((availability(domain:Bering, UNAVAIL)))
void unavailable_in_bering(void);

#undef UNAVAIL
#undef AVAIL
