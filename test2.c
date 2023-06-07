#include <termios.h>
#include <sys/ioctl.h>
#include <stddef.h>
#include <unistd.h>
#include <signal.h>
#include <stdio.h>

void main() {
	printf("%d", sizeof(FILE));
}
