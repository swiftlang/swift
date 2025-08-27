#include <availability_domain.h>

CLANG_ENABLED_AVAILABILITY_DOMAIN(Baltic);
CLANG_DISABLED_AVAILABILITY_DOMAIN(Mediterranean);

#define AVAIL 0
#define UNAVAIL 1

__attribute__((availability(domain:Baltic, AVAIL)))
void available_in_baltic(void);

#undef UNAVAIL
#undef AVAIL
