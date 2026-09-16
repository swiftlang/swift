#include "guards.h"

int useOrdinaryBindings() {
  auto value = Guards::Guarded::init(Guards::ordinary());
  value.setValue(7);
  return static_cast<int>(value.getValue());
}
