#include <signal.h>

void doResize(int dummy);

void setupSignals() {
  signal(SIGWINCH, doResize);
}
