void async_divide(double x, double y, void (* _Nonnull completionHandler)(double x))
  __attribute__((swift_attr("@macro_library.AddAsync")));

typedef struct SlowComputer {
} SlowComputer;

void computer_divide(const SlowComputer *computer, double x, double y, void (* _Nonnull completionHandler)(double x))
  __attribute__((swift_attr("@macro_library.AddAsync")))
  __attribute__((swift_name("SlowComputer.divide(self:_:_:completionHandler:)")));
