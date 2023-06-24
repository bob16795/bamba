#include <signal.h>
#include <stdio.h>

double glfwGetTime();

float getTime() {
  return (float)glfwGetTime();
}

//void Editor_doResize(int dummy);
//
//void setupSignals() {
//  signal(SIGWINCH, Editor_doResize);
//}
