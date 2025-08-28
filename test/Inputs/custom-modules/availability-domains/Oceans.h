#include <Rivers.h>
#include <availability_domain.h>

int arctic_pred(void);
int pacific_pred(void);

CLANG_DYNAMIC_AVAILABILITY_DOMAIN(Arctic, arctic_pred);
CLANG_DYNAMIC_AVAILABILITY_DOMAIN(Pacific, pacific_pred);

#define AVAIL 0
#define UNAVAIL 1

__attribute__((availability(domain:Arctic, AVAIL)))
void available_in_arctic(void);

__attribute__((availability(domain:Pacific, UNAVAIL)))
void unavailable_in_pacific(void);

__attribute__((availability(domain:Colorado, AVAIL)))
__attribute__((availability(domain:Pacific, AVAIL)))
void available_in_colorado_river_delta(void);

#undef UNAVAIL
#undef AVAIL
