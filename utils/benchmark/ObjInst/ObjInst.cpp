
#include <cstdio>
#include <cstdint>

extern "C" {
#include <mach/mach_time.h>
}

class Toggle {
public:
  bool state = true;
  Toggle(bool start_state) {
    state = start_state;
  }

  bool value() {
    return state;
  }

  Toggle *activate() {
    state = !state;
    return this ;
  }
};

class NthToggle : Toggle {
public:
  int count_max;
  int counter;

  NthToggle(bool start_state, int max_counter) : Toggle(start_state) {
    count_max = max_counter;
    counter = 0;
  }
  NthToggle *activate() {
    counter += 1;
    if (counter >= count_max) {
      state = !state;
      counter = 0;
    }
    return this;
  }
};

int main() {

  uint64_t start = mach_absolute_time();

  int n = 100000000;

  Toggle *toggle1 = new Toggle(true);
  //for (int i=0; i<5; i++) {
  //  Console.WriteLine((toggle1.activate().value()) ? "true" : "false");
  //}

  for (int i=0; i<n; i++) {
    Toggle *toggle = new Toggle(true);
    delete toggle;
  }
  //Console.WriteLine();


  //NthToggle ntoggle1 = new NthToggle(true, 3);
  //for (int i=0; i<8; i++) {
  //  Console.WriteLine((ntoggle1.activate().value()) ? "true" : "false");
  //}
  for (int i=0; i<n; i++) {
    NthToggle *toggle = new NthToggle(true, 3);
    delete toggle;
  }

  uint64_t end = mach_absolute_time() - start;

  printf("%llu nanoseconds.\n", end);

  return 0;
}
