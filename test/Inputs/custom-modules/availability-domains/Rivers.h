#include <availability_domain.h>

CLANG_DISABLED_AVAILABILITY_DOMAIN(Colorado);

__attribute__((deprecated("Use Colorado instead")))
CLANG_DISABLED_AVAILABILITY_DOMAIN(Grand);

#define AVAIL 0
#define UNAVAIL 1

__attribute__((availability(domain:Colorado, AVAIL)))
void available_in_colorado(void);

#undef UNAVAIL
#undef AVAIL
