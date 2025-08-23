#include <availability_domain.h>

CLANG_ENABLED_AVAILABILITY_DOMAIN(Salt);

#define AVAIL 0
#define UNAVAIL 1

__attribute__((availability(domain:Salt, AVAIL)))
void available_in_salt(void);

#undef UNAVAIL
#undef AVAIL
