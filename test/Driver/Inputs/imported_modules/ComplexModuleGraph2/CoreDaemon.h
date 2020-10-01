typedef void Runner(int);

struct Daemon {
  Runner *run;
};
